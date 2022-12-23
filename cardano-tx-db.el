;;; cardano-tx-db.el --- Connection to sqlite -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; This file is not part of GNU Emacs.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;;  Interface to database
;;
;;; Code:

(require 'org)
(require 'dired)
(require 'f)
(require 'dash)
(require 'yaml)
(require 'emacsql)
(require 'emacsql-sqlite)
(require 'subr-x)
(require 'json)
(require 'cardano-tx-log)
(require 'cardano-tx-utils)

(defcustom cardano-tx-db-keyring-dir "~/cardano-tx-wallet-keys"
  "Folder where to store all the database and key files."
  :type 'directory
  :group 'cardano-tx)

(defvar cardano-tx-db-connection nil)

;;; DB schema
(defconst cardano-tx-db-version 1)

(defconst cardano-tx-db--table-schema
  '((data
     [(datumhash :text :unique :primary-key) typed (datum TEXT)])
    (typed-files
     [(id integer :not-null :primary-key) type description (cbor-hex :unique) (path :unique)])
    (addresses
     [(id integer :not-null :primary-key) (raw :unique) spend-key stake-key monitor note])
    (tx-annotation
     [(txid :unique :primary-key) annotation])))

(defun cardano-tx-db--init (db)
  "Initiate tables on database DB."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table ,schema) cardano-tx-db--table-schema)
      (emacsql db [:create-table :if-not-exists $i1 $S2] table schema))
    (cardano-tx-db--set-user-version db 0))
  (cardano-tx-db--update db 0))

(defun cardano-tx-db--user-version (db)
  "Return current DB user version."
  (caar (emacsql db [:pragma user-version])))

(defun cardano-tx-db--set-user-version (db version)
  "Return to DB user VERSION."
  (emacsql db [:pragma (= user-version $s1)] version)
  version)

(defun cardano-tx-db--backup (annotation)
  "Copy current database as snapshot naming ANNOTATION."
  (emacsql-close cardano-tx-db-connection)
  (let* ((db-name (slot-value cardano-tx-db-connection 'file))
         (snapshot-name
          (format "%s-%s-%s.db"
                  (file-name-sans-extension db-name)
                  annotation
                  (format-time-string "%Y-%m-%dT%T"))))
    (cardano-tx-log 'info "Backing up %s %s" db-name snapshot-name)
    (copy-file db-name snapshot-name)
    ;; reconnect
    (setq cardano-tx-db-connection (emacsql-sqlite db-name))))

(defun cardano-tx-db--update (db version)
  "Update the database DB from VERSION to latest."
  (when (= version 0)
    (emacsql-with-transaction db
      (cardano-tx-log 'info "Upgrade from v0 to v1")
      (emacsql db [:create-table :if-not-exists master-keys $S1]
               [(id integer :not-null :primary-key)
                (fingerprint :unique)
                encrypted
                data
                note])
      (cardano-tx-db--set-user-version db (setq version 1)))))

(defun cardano-tx-db-upgrade-from (db)
  "From which version does DB needs upgrade."
  (let ((ver (cardano-tx-db--user-version db)))
    (when (< ver cardano-tx-db-version) ver)))

(defun cardano-tx-db-maybe-update (db)
  "Upgrade DB when needed."
  (when-let ((version (cardano-tx-db-upgrade-from db)))
    (-> (cardano-tx-db--backup (format "backup-v%d" version))
        (cardano-tx-db--update version))))

(defun cardano-tx-db ()
  "Retrieve active database."
  (unless (and cardano-tx-db-connection (emacsql-live-p cardano-tx-db-connection)
               (not (cardano-tx-db-upgrade-from cardano-tx-db-connection)))
    (let* ((db-path (expand-file-name "cardano.db" cardano-tx-db-keyring-dir))
           (connection (emacsql-sqlite db-path)))
      (condition-case nil
          (emacsql connection [:select * :from typed-files])
        (emacsql-error (cardano-tx-db--init connection)))
      (setq cardano-tx-db-connection connection)
      (cardano-tx-db-maybe-update connection)
      (cardano-tx-db-utxo-reset connection)))
  cardano-tx-db-connection)

;;; DB Table queries
(defun cardano-tx-db-retrieve-datum (datumhash)
  "Query database for DATUMHASH."
  (car
   (emacsql (cardano-tx-db)
            [:select [typed datum] :from data :where (= datumhash $s1)]
            datumhash)))

(defun cardano-tx-db-save-datum (datumhash typed datum)
  "Save in database TYPED DATUM under DATUMHASH."
  (emacsql (cardano-tx-db)
           [:insert-or-ignore :into data
            :values $v1]
           (vector datumhash typed datum)))

;;; Address management

(defun cardano-tx-db-stake-keys ()
  "Return stake verification keys."
  (cardano-tx-db-typed-files-where 'type "Stake%VerificationKeyShelley_ed25519%" 'like))


(defun cardano-tx-db-address--list ()
  "Return the list of all monitored addresses."
  (emacsql (cardano-tx-db) [:select [raw] :from addresses :where (= monitor 't)]))

(defun cardano-tx-db-insert-address (address &optional note spend-id stake-id monitor)
  "Insert into db ADDRESS with optionally a NOTE.

Specify also SPEND-ID and STAKE-ID if known from typed files.
MONITOR the address if not nil."
  (emacsql (cardano-tx-db)
           [:insert-or-ignore :into addresses :values $v1]
           (vector nil address spend-id stake-id monitor (or note ""))))

(defun cardano-tx-db-update-address-description (address description)
  "Update in db for ADDRESS its DESCRIPTION."
  (emacsql (cardano-tx-db)
           [:insert :into addresses
            [raw monitor note]
            :values $v1
            :on-conflict
            :do-update
            :set (= note $s2)]
           (vector address t description)
           description))

(defun cardano-tx-db-save-address-description (address skip-terminating-len)
  "Function to update in db for ADDRESS buffer content yet SKIP-TERMINATING-LEN."
  (lambda ()
    (interactive)
    (thread-last (buffer-substring-no-properties (point-min) (- (point-max) skip-terminating-len))
                 (string-trim)
                 (cardano-tx-db-update-address-description address))
    (kill-buffer)))

(defun cardano-tx-db-update-address-monitor (address monitor-p)
  "Update in db for ADDRESS its MONITOR-P state."
  (emacsql (cardano-tx-db)
           [:insert :into addresses
            [raw monitor]
            :values $v1
            :on-conflict
            :do-update
            :set (= monitor $s2)]
           (vector address monitor-p)
           monitor-p))

(defun cardano-tx-db-address-toggle-watch ()
  "Toggle the watch flag for registered address."
  (interactive)
  (let ((activep (-> (tabulated-list-get-entry) (aref 0) (string= "YES")))
        (address (-> (tabulated-list-get-entry) (aref 1))))
    (setf (aref (tabulated-list-get-entry) 0)
          (if activep "NO" "YES"))
    (cardano-tx-db-update-address-monitor address (not activep)))
  (forward-line)
  (tabulated-list-print t))

(defun cardano-tx-db-addresses--refresh ()
  "Obtain registered addresses from db and prepare them for table print."
  (->>
   (emacsql (cardano-tx-db)
            [:select [id monitor raw note] :from addresses])
   (mapcar (-lambda ((id mon addr note))
             (list id
                   (vector
                    (if mon "YES" "NO")
                    addr
                    (substring addr -9)
                    (or (car (split-string note "\n")) "")))))
   (setq tabulated-list-entries)))

(defun cardano-tx-db-address-copy ()
  "Copy address on entry to `kill-ring'."
  (interactive)
  (message "Address %s copied to `kill-ring'."
           (-> (tabulated-list-get-entry) (aref 1) (kill-new))))

(defun cardano-tx-db-address-annotate (address)
  "Annotate data for ADDRESS."
  (interactive
   (list (aref (tabulated-list-get-entry) 1)))
  (let ((result
         (car
          (emacsql (cardano-tx-db)
                   [:select [raw note sp:type sp:description sp:path rw:type rw:description rw:path] :from addresses
                    :left-join typed-files sp :on (= spend-key sp:id)
                    :left-join typed-files rw :on (= stake-key rw:id)
                    :where (= addresses:raw $s1)]
                   address))))
    (with-current-buffer (generate-new-buffer "*Address Annotation*")
      (setq-local header-line-format (format "Address: %s" address))
      (org-mode)
      (use-local-map (copy-keymap org-mode-map))
      (when result
        (insert (cadr result) "\n"))
      (thread-last
        (let ((cur (point)))
          (when result
            (apply #'cardano-tx-db-insert-file-annotation-block "Spending Condition" (seq-subseq result 2 5))
            (apply #'cardano-tx-db-insert-file-annotation-block "Reward withdraw Condition" (seq-subseq result 5))
            (add-text-properties cur (point-max) '(read-only t)))
          (- (point-max) cur))
        (cardano-tx-db-save-address-description address)
        (local-set-key "\C-c\C-c"))
      (goto-char (point-min))
      (switch-to-buffer (current-buffer)))))

(defconst cardano-tx-db-font-lock-keywords
  `((,(rx "NO") . font-lock-comment-face)
    (,(rx (or "spend:" "reward:") ) . font-lock-string-face)
    (,(rx "[" (= 8 hex) "]" ) . font-lock-builtin-face)))

(defvar cardano-tx-db-addresses-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'cardano-tx-db-address-toggle-watch)
    (define-key map "c" #'cardano-tx-db-address-copy)
    (define-key map "a" #'cardano-tx-db-address-annotate)
    map))

(define-derived-mode cardano-tx-db-addresses-mode tabulated-list-mode "Managed addresses"
  "Major mode for working with addresses."
  :interactive nil
  (setq tabulated-list-format [("Watch" 5 t)
                               ("Address" 20 t)
                               ("cksum" 9 t)
                               ("Note" 0 nil)])
  (setq font-lock-defaults (list cardano-tx-db-font-lock-keywords))
  (tabulated-list-init-header))

(defun cardano-tx-db-addresses ()
  "Open buffer with known addresses."
  (interactive)
  (with-current-buffer (get-buffer-create "*Cardano Addresses*")
    (cardano-tx-db-addresses-mode)
    (add-hook 'tabulated-list-revert-hook #'cardano-tx-db-addresses--refresh nil t)
    (cardano-tx-db-addresses--refresh)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; Typed Files
(defun cardano-tx-db-load-files (files)
  "Load into local database all FILES.

This reads the file and expects it to be a `cardano-cli' produced typed file."
  (-some->> files
    (mapcan (lambda (file)
              (when-let ((key-json
                          (or (ignore-errors (json-read-file file))
                              (ignore-errors
                                (with-temp-buffer
                                  (insert-file-contents file)
                                  (yaml-parse-string (buffer-substring-no-properties (point-min) (point-max))
                                                     :object-type 'alist))))))
                (if (member (alist-get 'type key-json) '("sig" "any" "all" "before" "after" "atLeast" ))
                    (list (vector nil "SimpleScriptV2"
                                  (file-name-base file)
                                  (json-encode key-json)
                                  file))
                  (list
                   (vector nil (alist-get 'type key-json)
                           (concat (file-name-base file) "\n"
                                   (alist-get 'description key-json))
                           (alist-get 'cborHex key-json)
                           file))))))
    (emacsql (cardano-tx-db)
             [:insert-or-ignore :into typed-files :values $v1])))

(defun cardano-tx-db-typed-files-where (column value &optional operator)
  "Filters the typed files table on COLUMN by VALUE using OPERATOR, defaults =."
  (emacsql (cardano-tx-db)
           `[:select [id type path description cbor-hex]
             :from typed-files :where (,(or operator '=) ,column '$s1)]
           value))

(defun cardano-tx-db-dired-load-files ()
  "Load into local database all Dired marked files.

This reads the file and expects it to be a `cardano-cli' produced typed file."
  (interactive)
  (cardano-tx-db-load-files (dired-get-marked-files)))

(defun cardano-tx-db-files--refresh ()
  "Query managed files from db and prepare them for table print."
  (->>
   (emacsql (cardano-tx-db)
            [:select [id type path description] :from typed-files])
   (mapcar (-lambda ((id type file-path description))
             (list id
                   (vector
                    type
                    (file-name-nondirectory (or file-path ""))
                    (or (car (split-string description "\n")) "")))))
   (setq tabulated-list-entries)))

(defun cardano-tx-db-file-open (file-id)
  "Open file of FILE-ID."
  (interactive
   (list (tabulated-list-get-id)))
  (-some-> (cardano-tx-db-typed-files-where 'id file-id) (car) (elt 2) (find-file)))

(defun cardano-tx-db-insert-native-script-block (type file-path)
  "Insert an `org-mode' JS block containing FILE-PATH if TYPE is `SimpleScriptV2'."
  (when (string= type "SimpleScriptV2")
    (insert "\n#+begin_src js\n"
            (with-temp-buffer
              (insert-file-contents file-path)
              (json-pretty-print-buffer)
              (buffer-string))
            "\n#+end_src")))

(defun cardano-tx-db-insert-file-annotation-block (headline type description file-path)
  "Insert annotation with HEADLINE about FILE-PATH of TYPE and DESCRIPTION."
  (when type
    (insert "\n* " headline " " type "\n" description "\n")
    (cardano-tx-db-insert-native-script-block type file-path)))

(defun cardano-tx-db-file-write (file-id &optional filename)
  "Write file of FILE-ID to FILENAME.
Which defaults to know location or description on keyring."
  (interactive (list (tabulated-list-get-id) nil))
  (when-let ((result (car (cardano-tx-db-typed-files-where 'id file-id))))
    (seq-let (file-id type path description cbor-hex) result
      (let ((filename
             (cardano-tx-clean-filename
              (or filename path
                  (expand-file-name
                   (cardano-tx-escape-non-alphanum-bracket
                    (car (split-string description "\n" t  "[[:space:]]")))
                   cardano-tx-db-keyring-dir))
              cardano-tx-db-keyring-dir)))
        (emacsql (cardano-tx-db)
                 [:update typed-files
                  :set (= path $s1)
                  :where (= id $s2)]
                 filename
                 file-id)
        (f-write
         (json-serialize
          (list :type type
                :description description
                :cborHex cbor-hex))
         'utf-8 filename)
        (when (eq (with-current-buffer (current-buffer)
                    major-mode)
                  'cardano-tx-db-files-mode)
          (cardano-tx-db-files--refresh)
          (tabulated-list-print t))))))

(defun cardano-tx-db-file-delete (file-id)
  "Delete data for FILE-ID. Optionally file too."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let ((result (car (cardano-tx-db-typed-files-where 'id file-id))))
    (seq-let (file-id _type path description) result
      (emacsql (cardano-tx-db)
               [:delete :from typed-files
                :where (= id $s1)]
               file-id)
      (when (and path (file-exists-p path)
                 (yes-or-no-p
                  (format "Deleting entry %d '%s' from Database.\nDo you want to remove the file %s too?"
                          file-id description path)))
        (delete-file path))
      (when (eq (with-current-buffer (current-buffer)
                  major-mode)
                'cardano-tx-db-files-mode)
        (forward-line)
        (cardano-tx-db-files--refresh)
        (tabulated-list-print t)))))

(defun cardano-tx-db-file-name-copy (file-id)
  "Copy path for FILE-ID into `kill-ring'."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let ((result (car (cardano-tx-db-typed-files-where 'id file-id))))
    (seq-let (_id _type path) result
      (when path
        (message "Filename '%s' copied to `kill-ring'." (kill-new path))))))

(defun cardano-tx-db-file-annotate (file-id)
  "Annotate data for FILE-ID."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let ((result (car (cardano-tx-db-typed-files-where 'id file-id))))
    (with-current-buffer (generate-new-buffer "*File description*")
      (setq-local header-line-format (format "Description of file: %s" (elt result 2)))
      (org-mode)
      (insert (elt result 3) "\n")
      (let ((cur (point)))
        (cardano-tx-db-insert-native-script-block (elt result 1) (elt result 2))
        (add-text-properties cur (point-max) '(read-only t))
        (goto-char cur)
        (let ((script-length (- (point-max) cur)))
          (use-local-map (copy-keymap org-mode-map))
          (local-set-key "\C-c\C-c"
                         (lambda ()
                           (interactive)
                           (emacsql (cardano-tx-db)
                                    [:update typed-files
                                     :set (= description $s1)
                                     :where (= id $s2)]
                                    (string-trim
                                     (buffer-substring-no-properties (point-min)
                                                                     (- (point-max) script-length)))
                                    file-id)
                           (kill-buffer)))))
      (switch-to-buffer (current-buffer)))))

(defvar cardano-tx-db-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'cardano-tx-db-file-annotate)
    (define-key map "o" #'cardano-tx-db-file-open)
    (define-key map "w" #'cardano-tx-db-file-write)
    (define-key map "c" #'cardano-tx-db-file-name-copy)
    (define-key map "d" #'cardano-tx-db-file-delete)
    map))

(define-derived-mode cardano-tx-db-files-mode tabulated-list-mode "Managed files"
  "Major mode for working with addresses."
  :interactive nil
  (setq tabulated-list-format [("Type" 15 t)
                               ("Filename" 18 t)
                               ("Annotation" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'cardano-tx-db-files--refresh nil t)
  (setq font-lock-defaults (list cardano-tx-db-font-lock-keywords))
  (tabulated-list-init-header))

(defun cardano-tx-db-typed-files ()
  "Open buffer listing managed files."
  (interactive)
  (with-current-buffer (get-buffer-create "*Cardano managed files*")
    (cardano-tx-db-files-mode)
    (cardano-tx-db-files--refresh)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; UTxOs

(defun cardano-tx-db-utxo-reset (db)
  "Clear and create temporary UTxO database on DB."
  (emacsql db [:drop :table :if-exists utxos])
  (emacsql db [:create :temporary :table utxos
                       ([(utxo :unique) addr-id datumhash lovelaces assets ])]))

(defun cardano-tx-db-utxo--list ()
  "Return all known UTxOs."
  (emacsql (cardano-tx-db) [:select * :from utxos]))

(defun cardano-tx-db-utxo-load (utxos-list)
  "Load on temporary UTxO database the UTXOS-LIST."
  ;;https://dba.stackexchange.com/questions/46410/how-do-i-insert-a-row-which-contains-a-foreign-key/46415#46415
  (-some->> utxos-list
    (mapcar (-lambda ((utxo . info))
              (vector utxo
                      (cardano-tx-get-in info "address")
                      (cardano-tx-get-in info "datumhash")
                      (cardano-tx-get-in info "value" "lovelace")
                      (cl-remove-if
                       (lambda (val) (string= (car val) "lovelace"))
                       (cardano-tx-get-in info "value")))))
    (emacsql (cardano-tx-db)
             [:with ins [utxo addr datumhash lovelaces assets] :as [:values $v1]
              :insert-into utxos [utxo addr-id datumhash lovelaces assets]
              :select [ins:utxo addresses:id datumhash ins:lovelaces assets]
              :from addresses
              :join ins :on (= ins:addr addresses:raw)])))

(defun cardano-tx-db-utxo-info (&optional v-utxos)
  "Obtain known data for V-UTXOS vector otherwise all UTxOs."
  (emacsql (cardano-tx-db)
           (vconcat [:select [utxo raw lovelaces assets data:datumhash datum note] :from utxos
                     :left-join data :on (= data:datumhash utxos:datumhash)
                     :join addresses :on (= utxos:addr-id addresses:id)]
                    (unless (seq-empty-p v-utxos)
                      [:where (in utxo $v1)])
                    [:order-by [(asc lovelaces)]])
           v-utxos))

(defun cardano-tx-db-utxo-spend (v-utxos)
  "Obtain known spend data for V-UTXOS vector."
  (emacsql (cardano-tx-db)
           [:select [utxo type path typed datum] :from utxos
            :left-join data :on (= data:datumhash utxos:datumhash)
            :join addresses :on (= utxos:addr-id addresses:id)
            :left-join typed-files :on (= spend-key typed-files:id) ;; Left-join because address could just be inserted
            :where (in utxo $v1)]
           v-utxos))

(provide 'cardano-tx-db)
;;; cardano-tx-db.el ends here
