;;; cardano-db.el --- Connection to sqlite -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") (yaml "0.1.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Interface to database
;;
;;; Code:

(require 'org)
(require 'dired)
(require 'dash)
(require 'yaml)
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'subr-x)
(require 'json)
(require 'cardano-utils)

(defgroup cardano-db nil
  "Database functionalities."
  :group 'cardano)

(defcustom cardano-db-keyring-dir "~/cardano-wallet-keys"
  "Folder where to store all the database and key files."
  :type 'directory)

(defconst cardano-db-version 0)

(defconst cardano-db--table-schema
  '((data
     [(datumhash :text :unique :primary-key) typed (datum TEXT)])
    (typed-files
     [(id integer :not-null :primary-key) type description (cbor-hex :unique) (path :unique)])
    (addresses
     [(id integer :not-null :primary-key) (raw :unique) spend-key stake-key monitor note])
    (tx-annotation
     [(txid :unique :primary-key) annotation])))

(defvar cardano-db-connection nil)

(defun cardano-db--init (db)
  "Initiate tables on database DB."
  (emacsql-with-transaction db
    (pcase-dolist (`(,table ,schema) cardano-db--table-schema)
      (emacsql db [:create-table :if-not-exists $i1 $S2] table schema))
    (emacsql db (format "PRAGMA user_version = %s" cardano-db-version))))

(defun cardano-db ()
  "Retrieve active database."
  (unless (and cardano-db-connection (emacsql-live-p cardano-db-connection))
    (let* ((db-path (expand-file-name "cardano.db" cardano-db-keyring-dir))
           (connection (emacsql-sqlite3 db-path)))
      (condition-case nil
          (emacsql connection [:select * :from typed-files])
        (emacsql-error (cardano-db--init connection)))
      (setq cardano-db-connection connection)
      (cardano-db-utxo-reset)))
  cardano-db-connection)

(defun cardano-db-retrieve-datum (datumhash)
  "Query database for DATUMHASH."
  (car
   (emacsql (cardano-db)
            [:select [typed datum] :from data :where (= datumhash $s1)]
            datumhash)))

(defun cardano-db-save-datum (datumhash typed datum)
  "Save in database TYPED DATUM under DATUMHASH."
  (emacsql (cardano-db)
           [:insert-or-ignore :into data
            :values $v1]
           (vector datumhash typed datum)))

;;; Address management

(defun cardano-db-stake-keys ()
  "Return stake verification keys."
  (cardano-db-files-of-type  "StakeVerificationKeyShelley_ed25519"))

(defun cardano-db-address--list ()
  "Return the list of all monitored addresses."
  (emacsql (cardano-db)
           [:select [raw path] :from addresses
            :left-join typed-files :on (= spend-key typed-files:id)
            :where (= monitor 't)]))

(defun cardano-db-insert-address (address &optional note spend-id stake-id monitor)
  "Insert into db ADDRESS with optionally a NOTE.

Specify also SPEND-ID and STAKE-ID if known from typed files.
MONITOR the address if not nil."
  (emacsql (cardano-db)
           [:insert-or-ignore :into addresses
            :values $v1]
           (vector nil address spend-id stake-id monitor (or note ""))))

(defun cardano-db-address-toggle-watch ()
  "Toggle the watch flag for registered address."
  (interactive)
  (let ((activep (-> (tabulated-list-get-entry) (aref 0) (string= "YES")))
        (address (-> (tabulated-list-get-entry) (aref 1)))
        (note (-> (tabulated-list-get-entry) (aref 3) (substring-no-properties))))
    (setf (aref (tabulated-list-get-entry) 0)
          (if activep "NO" "YES"))
    (emacsql (cardano-db)
             [:insert :into addresses
              [raw monitor note]
              :values $v1
              :on-conflict
              :do-update
              :set (= monitor $s2)]
             (vector address (not activep) note)
             (not activep)))
  (forward-line)
  (tabulated-list-print t))

(defun cardano-db-addresses--refresh ()
  "Obtain registered addresses from db and prepare them for table print."
  (->>
   (emacsql (cardano-db)
            [:select [id monitor raw note] :from addresses])
   (mapcar (-lambda ((id mon addr note))
             (list id
                   (vector
                    (if mon "YES" "NO")
                    addr
                    (substring addr -9)
                    (or (car (split-string note "\n")) "")))))
   (setq tabulated-list-entries)))

(defun cardano-db-address-copy ()
  "Copy address on entry to `kill-ring'."
  (interactive)
  (message "Address %s copied to `kill-ring'."
           (-> (tabulated-list-get-entry) (aref 1) (kill-new))))

(defun cardano-db-address-annotate (address-id)
  "Annotate data for ADDRESS-ID."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let ((result
              (car
               (emacsql (cardano-db)
                        [:select [raw note sp:type sp:description sp:path rw:type rw:description rw:path] :from addresses
                         :left-join typed-files sp :on (= spend-key sp:id)
                         :left-join typed-files rw :on (= stake-key rw:id)
                         :where (= addresses:id $s1)]
                        address-id))))
    (with-current-buffer (generate-new-buffer "*Address Annotation*")
      (setq-local header-line-format (format "Address: %s" (car result)))
      (org-mode)
      (insert (cadr result) "\n")
      (let ((cur (point)))
        (apply #'cardano-db-insert-file-annotation-block "Spending Condition" (seq-subseq result 2 5))
        (apply #'cardano-db-insert-file-annotation-block "Reward withdraw Condition" (seq-subseq result 5))
        (add-text-properties cur (point-max) '(read-only t))
        (goto-char cur)
        (let ((description-length (- (point-max) cur)))
          (local-set-key "\C-c\C-c"
                         (lambda ()
                           (interactive)
                           (emacsql (cardano-db)
                                    [:update addresses
                                     :set (= note $s1)
                                     :where (= id $s2)]
                                    (string-trim
                                     (buffer-substring-no-properties (point-min)
                                                                     (- (point-max) description-length)))
                                    address-id)
                           (kill-buffer)))))
      (switch-to-buffer (current-buffer)))))

(defvar cardano-db-addresses-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w" #'cardano-db-address-toggle-watch)
    (define-key map "c" #'cardano-db-address-copy)
    (define-key map "a" #'cardano-db-address-annotate)
    map))

(define-derived-mode cardano-db-addresses-mode tabulated-list-mode "Managed addresses"
  "Major mode for working with addresses."
  :interactive nil
  (setq tabulated-list-format [("Watch" 5 t)
                               ("Address" 20 t)
                               ("cksum" 9 t)
                               ("Note" 0 nil)])
  (tabulated-list-init-header))

(defun cardano-db-addresses ()
  "Open buffer with known addresses."
  (interactive)
  (with-current-buffer (get-buffer-create "*Cardano Addresses*")
    (cardano-db-addresses-mode)
    (add-hook 'tabulated-list-revert-hook #'cardano-db-addresses--refresh nil t)
    (cardano-db-addresses--refresh)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; Typed Files
(defun cardano-db-load-files (files)
  "Load into cardano-db all FILES.

This reads the file and expects it to be a cardano-cli produced typed file."
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
    (emacsql (cardano-db)
             [:insert-or-ignore :into typed-files :values $v1])))

(defun cardano-db-files-of-type (type)
  "Return all files of TYPE."
  (emacsql (cardano-db)
           [:select [id path description]
            :from typed-files :where (= type $s1)]
           type))

(defun cardano-db-dired-load-files ()
  "Load into cardano-db all Dired marked files.

This reads the file and expects it to be a cardano-cli produced typed file."
  (interactive)
  (cardano-db-load-files (dired-get-marked-files)))

(defun cardano-db-files--refresh ()
  "Query managed files from db and prepare them for table print."
  (->>
   (emacsql (cardano-db)
            [:select [id type path description] :from typed-files])
   (mapcar (-lambda ((id type file-path description))
             (list id
                   (vector
                    type
                    (file-name-nondirectory  file-path)
                    (or (car (split-string description "\n")) "")))))
   (setq tabulated-list-entries)))

(defun cardano-db-file (file-id)
  "Return file & description for FILE-ID."
  (car
   (emacsql (cardano-db)
            [:select [type description path] :from typed-files :where (= id $s1)]
            file-id)))

(defun cardano-db-file-open (file-id)
  "Open file of FILE-ID."
  (interactive
   (list (tabulated-list-get-id)))
  (-some-> (cardano-db-file file-id) (elt 2) (find-file)))

(defun cardano-db-insert-native-script-block (type file-path)
  "Insert an `org-mode' JS block containing FILE-PATH if TYPE is SimpleScriptV2."
  (when (string= type "SimpleScriptV2")
    (insert "\n#+begin_src js\n"
            (with-temp-buffer
              (insert-file-contents file-path)
              (json-pretty-print-buffer)
              (buffer-string))
            "\n#+end_src")))

(defun cardano-db-insert-file-annotation-block (headline type description file-path)
  "Insert annotation with HEADLINE about FILE-PATH of TYPE and DESCRIPTION."
  (when type
    (insert "\n* " headline " " type "\n" description "\n")
    (cardano-db-insert-native-script-block type file-path)))

(defun cardano-db-file-annotate (file-id)
  "Annotate data for FILE-ID."
  (interactive
   (list (tabulated-list-get-id)))
  (when-let ((result (cardano-db-file file-id)))
    (with-current-buffer (generate-new-buffer "*File description*")
      (setq-local header-line-format (format "Description of file: %s" (elt result 2)))
      (org-mode)
      (insert (cadr result) "\n")
      (let ((cur (point)))
        (cardano-db-insert-native-script-block (caddr result) (car result))
        (add-text-properties cur (point-max) '(read-only t))
        (goto-char cur)
        (let ((script-length (- (point-max) cur)))
          (local-set-key "\C-c\C-c"
                         (lambda ()
                           (interactive)
                           (emacsql (cardano-db)
                                    [:update typed-files
                                     :set (= description $s1)
                                     :where (= id $s2)]
                                    (string-trim
                                     (buffer-substring-no-properties (point-min)
                                                                     (- (point-max) script-length)))
                                    file-id)
                           (kill-buffer)))))
      (switch-to-buffer (current-buffer)))))

(defvar cardano-db-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'cardano-db-file-annotate)
    (define-key map "o" #'cardano-db-file-open)
    map))

(define-derived-mode cardano-db-files-mode tabulated-list-mode "Managed files"
  "Major mode for working with addresses."
  :interactive nil
  (setq tabulated-list-format [("Type" 15 t)
                               ("Filename" 18 t)
                               ("Annotation" 0 t)])
  (add-hook 'tabulated-list-revert-hook #'cardano-db-files--refresh nil t)
  (tabulated-list-init-header))

(defun cardano-db-typed-files ()
  "Open buffer listing managed files."
  (interactive)
  (with-current-buffer (get-buffer-create "*Cardano managed files*")
    (cardano-db-files-mode)
    (cardano-db-files--refresh)
    (tabulated-list-print)
    (pop-to-buffer (current-buffer))))

;;; UTxOs

(defun cardano-db-utxo-reset ()
  "Clear and create temporary UTxO database."
  (emacsql (cardano-db) [:drop :table :if-exists utxos])
  (emacsql (cardano-db)
           [:create :temporary :table utxos
            ([(utxo :unique) addr-id datumhash lovelaces assets ])]))

(defun cardano-db-utxo--list ()
  "Return all known UTxOs."
  (emacsql (cardano-db) [:select * :from utxos]))

(defun cardano-db-utxo-load (utxos-list)
  "Load on temporary UTxO database the UTXOS-LIST."
  ;;https://dba.stackexchange.com/questions/46410/how-do-i-insert-a-row-which-contains-a-foreign-key/46415#46415
  (-some->> utxos-list
    (mapcar (-lambda ((utxo . info))
              (vector utxo
                      (cardano-utils-get-in info "address")
                      (cardano-utils-get-in info "datumhash")
                      (cardano-utils-get-in info "value" "lovelace")
                      (cl-remove-if
                       (lambda (val) (string= (car val) "lovelace"))
                       (cardano-utils-get-in info "value")))))
    (emacsql (cardano-db)
             [:with ins [utxo addr datumhash lovelaces assets] :as [:values $v1]
              :insert-into utxos [utxo addr-id datumhash lovelaces assets]
              :select [ins:utxo addresses:id datumhash ins:lovelaces assets]
              :from addresses
              :join ins :on (= ins:addr addresses:raw)])))

(defun cardano-db-utxo-info (&optional v-utxos)
  "Obtain known data for V-UTXOS vector otherwise all UTXoS."
  (emacsql (cardano-db)
           (vconcat [:select [utxo raw lovelaces assets data:datumhash datum] :from utxos
                     :left-join data :on (= data:datumhash utxos:datumhash)
                     :join addresses :on (= utxos:addr-id addresses:id)]
                    (unless (seq-empty-p v-utxos)
                      [:where (in utxo $v1)])
                    [:order-by [(asc lovelaces)]
                     ])
           v-utxos))

(defun cardano-db-utxo-spend (v-utxos)
  "Obtain known spend data for V-UTXOS vector."
  (emacsql (cardano-db)
           [:select [utxo type path typed datum] :from utxos
            :left-join data :on (= data:datumhash utxos:datumhash)
            :join addresses :on (= utxos:addr-id addresses:id)
            :join typed-files :on (= spend-key typed-files:id)
            :where (in utxo $v1)]
           v-utxos))

(provide 'cardano-db)
;;; cardano-db.el ends here
