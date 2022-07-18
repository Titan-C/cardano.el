;;; cardano-tx.el --- Cardano transaction editor -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Oscar Najera <https://oscarnajera.com>
;; Maintainer: Oscar Najera <hi@oscarnajera.com>
;; Version: 0.1.0
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "27.1") (f "0.20.0") (yasnippet "0.14.0") (yaml-mode "0.0.15") (yaml "0.1.0") (helm "3.6.2") (cbor "0.2.0") (bech32 "0.1.0") (readable-numbers "0.1.0") (emacsql "3.0.0") (emacsql-sqlite3 "1.0.2") )

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

;;; Commentary:
;;
;; Wrapping around cardano-cli to manage transactions
;;
;;; Code:

(require 'dash)
(require 'helm)
(require 'rx)
(require 'subr-x)
(require 'yaml-mode)
(require 'json)
(require 'yaml)
(require 'yasnippet)
(require 'readable-numbers)
(require 'cardano-tx-cli)
(require 'cardano-tx-db)
(require 'cardano-tx-address)
(require 'cardano-tx-assets)
(require 'cardano-tx-utils)

(defconst cardano-tx-snippet-dir
  (concat (file-name-directory (or load-file-name buffer-file-name)) "snippets"))


(defvar-local cardano-tx--buffer nil
  "Buffer containing the transaction specification.
This is only available on TX preview buffers.")

(defun cardano-tx-rewards (address)
  "Recover the rewards info sitting at ADDRESS."
  (interactive (list (read-string "Stake address: "
                                  (thing-at-point 'symbol))))
  (let ((rewards-file (make-temp-file "rewards-" nil ".json"))
        (json-key-type 'string))
    (cardano-tx-cli "query" "stake-address-info" "--out-file" rewards-file
                    "--address" address)
    (when-let ((result
                (cardano-tx-get-in (json-read-file rewards-file) 0)))
      (when (called-interactively-p 'interactive)
        (cardano-tx-cli-pretty-yaml-message result))
      (kill-new
       (number-to-string
        (cardano-tx-get-in result 'rewardAccountBalance))))))

(defun cardano-tx-utxos (addresses)
  "Recover the utxos sitting at ADDRESSES."
  (let ((utxos-file (make-temp-file "utxos-" nil ".json")))
    (apply #'cardano-tx-cli "query" "utxo" "--out-file" utxos-file
           (--mapcat (list "--address" it) addresses))
    (cardano-tx-db-utxo-reset)
    (let ((json-key-type 'string))
      (cardano-tx-db-utxo-load (json-read-file utxos-file)))))

(defun cardano-tx-show-datum (datumhash)
  "Recollect out of DATUMHASH from DB and return as string."
  (concat "datumhash: " datumhash
          (if-let ((data (cardano-tx-db-retrieve-datum datumhash)))
              (format "\ndata: %s" (cadr data))
            (propertize "\nUnknown Data" 'face 'font-lock-keyword-face))))

(defun cardano-tx--utxo-contents (utxo-row)
  "Human readable contents of UTXO-ROW."
  (-let (((utxo addr lovelaces assets datumhash datum addr-note) utxo-row))
    (-> (list
         utxo
         addr
         addr-note
         (cardano-tx-assets-format-tokens
          (cl-acons "lovelace" lovelaces assets))
         (-some->> datumhash (format "datumhash: %s" ))
         (-some->> datum (format "data: %s" )))
        (string-join "\n")
        (string-trim))))


(defun cardano-tx-utxo-entry (utxo spend-type script-path typed datum)
  "Builds an input entry of UTXO.

SPEND-TYPE is the spending condition, it is important for simple or Plutus
scripts.  SCRIPT-PATH is the corresponding unlocking script.
Would UTxO have a DATUM include according to TYPED."
  (->
   (append (list utxo)
           (when (member spend-type '("SimpleScriptV2" "PlutusScriptV1"))
             (list (format "script-file: %s" script-path)))
           (and datum
                (list (if typed
                          (format "datumfile: %s" (make-temp-file "datum-" nil ".json" datum))
                        (format "datum: %S" datum))
                      "redeemer:")))
   (string-join "\n    ")
   (string-trim-right)))

(defun cardano-tx-helm-utxos (reset)
  "Pick from wallet controlled utxos and put them on kill ring.
If RESET query the node again."
  (interactive "P")
  (when (or reset (null (cardano-tx-db-utxo-info)))
    (cardano-tx-utxos (mapcar #'car (cardano-tx-db-address--list))))
  (--> (mapcar #'cardano-tx--utxo-contents (cardano-tx-db-utxo-info))
       (cardano-tx-pick "Select UTXOS" it)
       (cl-map 'vector (lambda (text) (-> (split-string text "\n") (car) (substring-no-properties))) it)
       (cardano-tx-db-utxo-spend it)
       (mapconcat (lambda (row) (apply #'cardano-tx-utxo-entry row)) it "\n  - utxo: ")
       (kill-new it)))

(defun cardano-tx-witness-query (witness-list)
  "SQLite query union to include rows about explicitly declared in WITNESS-LIST."
  (if-let ((witness (mapcar (lambda (str) `(like path ,(concat "%" str "%" ".vkey"))) witness-list)))
      `[:union :select [path] :from typed-files
        :where ,(if (> (length witness) 1)
                    `(or ,@witness)
                  (car witness))]
    []))

(defun cardano-tx-witnesses (input-data)
  "Given the INPUT-DATA about to be spent.  Which wallets control them?
All the wallet address-file pairs in the keyring are tested."
  (->> (vconcat (cardano-tx-get-in input-data 'collateral)
                (--map (cardano-tx-get-in it 'utxo)
                       (cardano-tx-get-in input-data 'inputs)))
       (emacsql (cardano-tx-db)
                (vconcat
                 [:select :distinct [path] :from utxos
                  :join addresses :on (= addr-id addresses:id)
                  :join typed-files :on (= spend-key typed-files:id)
                  :where (and (= type "PaymentVerificationKeyShelley_ed25519") (in utxo $v1))]
                 (cardano-tx-witness-query
                  (cardano-tx-get-in input-data 'witness))))
       (--map (replace-regexp-in-string ".vkey$" ".skey" (car it)))))

(defun cardano-tx-sign (tx-file witness-keys)
  "Sign a transaction file TX-FILE with WITNESS-KEYS."
  (let ((signed-file (concat tx-file ".signed")))
    (->> (list "transaction" "sign"
               "--tx-body-file" tx-file
               (--map (list "--signing-key-file" it) witness-keys)
               "--out-file" signed-file)
         (flatten-tree)
         (apply #'cardano-tx-cli))
    signed-file))

(defun cardano-tx-hash-script-data (datum)
  "Hash the DATUM."
  (cardano-tx-cli "transaction" "hash-script-data" "--script-data-value" (json-encode datum)))

(defun cardano-tx-hash-script-data-file (datumfile)
  "Hash the DATUMFILE."
  (cardano-tx-cli "transaction" "hash-script-data" "--script-data-file" datumfile))

(defun cardano-tx-datum-hash (tx-out)
  "Obtain the datum hash from TX-OUT.  Calculate from datum field if needed."
  (or (cardano-tx-nw-p (cardano-tx-get-in tx-out "datumhash"))
      (-some-> (cardano-tx-get-in tx-out "datum")
        cardano-tx-hash-script-data)
      (-some-> (cardano-tx-get-in tx-out "datumfile")
        expand-file-name
        cardano-tx-hash-script-data-file)))

(defun cardano-tx-save-datum (tx-out)
  "Save in database new data from TX-OUT."
  (when-let ((datum (cardano-tx-get-in tx-out "datum")))
    (cardano-tx-db-save-datum
     (cardano-tx-hash-script-data datum) nil datum))
  (when-let* ((datumfile (cardano-tx-get-in tx-out "datumfile"))
              (file-path (expand-file-name datumfile)))
    (cardano-tx-db-save-datum
     (cardano-tx-hash-script-data-file file-path)
     t
     (f-read file-path))))

(defun cardano-tx--value-amount (value)
  "Create sum string of output amount for VALUE."
  (-> (--map (if (numberp it)
                 (number-to-string it)
               (-let (((amount policy tokenname) it))
                 (format "%d %s.%s" amount policy (cbor-string->hexstring tokenname))))
             (cardano-tx-assets-flatten value))
      (sort #'string>) ;; Sorting for lovelace amount first
      (string-join "+")))

(defun cardano-tx--format-addr-amount (output)
  "Format OUTPUT Lisp object with keys address & amount for CLI."
  (let ((address (cardano-tx-get-in output 'address))
        (amount  (cardano-tx-get-in output 'amount)))
    (concat address "+" (cardano-tx--value-amount amount))))

(defun cardano-tx--out-args (tx-out)
  "Generate the command line arguments for TX-OUT."
  (when-let ((target-addr (cardano-tx-nw-p (cardano-tx-get-in tx-out 'address))))
    (if (cardano-tx-get-in tx-out 'change)
        (list "--change-address" target-addr)
      (list "--tx-out" (cardano-tx--format-addr-amount tx-out)
            (-some->> (cardano-tx-datum-hash tx-out)
              (list "--tx-out-datum-hash"))))))

(defun cardano-tx-save-native-script (native-script)
  "Convert a NATIVE-SCRIPT object to JSON-file."
  (let ((script (make-temp-file "native-script" nil)))
    (f-write (json-encode native-script) 'utf-8 script)
    (let ((new-name (concat (file-name-directory script) (cardano-tx-policyid script) ".script")))
      (rename-file script new-name 'overwrite)
      new-name)))

(defun cardano-tx--translate-mint-policy (mint-script)
  "Convert a MINT-SCRIPT object to JSON-file or pass the JSON-file."
  (if (and (stringp mint-script) (string-suffix-p ".script" mint-script))
      mint-script
    (cardano-tx-save-native-script mint-script)))

(defun cardano-tx-policyid (mint-script-file)
  "Calculate the policy id for MINT-SCRIPT-FILE."
  (interactive (list (read-file-name "Which policy script: ")))
  (kill-new (cardano-tx-cli "transaction" "policyid" "--script-file" mint-script-file)))

(defun cardano-tx--mint-rows (mints)
  "From the MINTS alist return rows of:

\(name policy-id json-policy-file assets\)"
  (mapcar (-lambda ((policy-name . conditions))
            (let ((policy-file (cardano-tx--translate-mint-policy (cardano-tx-get-in conditions 'policy))))
              (list policy-name
                    (cardano-tx-policyid policy-file)
                    policy-file
                    (cardano-tx-get-in conditions 'assets))))
          mints))

(defun cardano-tx--mints (mint-rows)
  "Generate the mint command options given the MINT-ROWS."
  (if-let ((value (--map (cons (cadr it) (cadddr it)) mint-rows)))
      (list "--mint"
            (cardano-tx--value-amount value)
            (seq-map (-lambda ((_ _ scriptfile))
                       (list "--mint-script-file" scriptfile))
                     mint-rows))))

(defun cardano-tx--replace-mint-asset-names (mint-rows)
  "Return function to replace minted assets variable names.

It produces the actual policy-id from the MINT-ROWS."
  (let ((replace-table (--map (cons (car it) (cadr it)) mint-rows)))
    (lambda (input)
      (alist-get input replace-table input nil #'string=))))

(defun cardano-tx--data-retrieve (data-type tx-in)
  "Obtain the DATA-TYPE either \"datum\" or \"redeemer\" for TX-IN."
  (or (-some->> data-type intern (cardano-tx-get-in tx-in) json-encode
                (list (format "--tx-in-%s-value" data-type)))
      (-some->> (concat data-type "file") intern (cardano-tx-get-in tx-in) expand-file-name
                (list (format "--tx-in-%s-file" data-type)))))

(defun cardano-tx--in-args (tx-in)
  "Generate the command line arguments for TX-IN."
  (list "--tx-in" (cardano-tx-get-in tx-in 'utxo)
        (-some--> (cardano-tx-get-in tx-in 'script-file) cardano-tx-nw-p expand-file-name
                  (list "--tx-in-script-file" it
                        (-> "datum"    (cardano-tx--data-retrieve tx-in))
                        (-> "redeemer" (cardano-tx--data-retrieve tx-in))))))

(defun cardano-tx--plutus-args (input-data)
  "Extra arguments when dealing with Plutus smart contacts, based on INPUT-DATA."
  (when-let ((tx-in-collateral (cardano-tx-get-in input-data 'collateral))
             (params-file (make-temp-file "cardano-params" nil ".json")))
    (cardano-tx-cli "query" "protocol-parameters" "--out-file" params-file)
    (list "--tx-in-collateral" tx-in-collateral
          "--protocol-params-file" params-file)))

(defun cardano-tx--metadata-args (metadata)
  "Generate metadata command for METADATA."
  (let ((metadata-file (make-temp-file "metadata" nil ".json")))
    (f-write (json-encode metadata) 'utf-8 metadata-file)
    (list "--metadata-json-file" metadata-file) ))

(defun cardano-tx--validity-interval (validity-interval)
  "Generate validity interval command arguments from VALIDITY-INTERVAL object."
  (--map
   (-some->> (cardano-tx-get-in validity-interval (concat "invalid-" it))
     number-to-string (list (concat "--invalid-" it)))
   '("before" "hereafter")))

(defun cardano-tx--withdrawals (withdrawals)
  "Instructions for WITHDRAWALS."
  (--map (list "--withdrawal" (cardano-tx--format-addr-amount it)) withdrawals))

(defun cardano-tx--certificates (certificates)
  "Instructions to upload CERTIFICATES."
  (--map
   (-some->> (or (cardano-tx-get-in it 'file)
                 (cardano-tx--registration-cert it)
                 (cardano-tx--delegation-cert it))
     (list "--certificate-file"))
   certificates))

(defun cardano-tx--registration-cert (cert)
  "Return registration certificate file or create it if needed from object CERT."
  (let ((default-stake-key (cadar (cardano-tx-db-stake-keys))))
    (pcase (cardano-tx-get-in cert 'registration)
      ((pred null) nil)
      (:null (-some-> default-stake-key (cardano-tx-address-stake-registration-cert)))
      ((and conf (pred listp))
       (-some-> (or (cardano-tx-get-in conf 'vkey-file) default-stake-key)
         (cardano-tx-address-stake-registration-cert
          (cardano-tx-get-in conf 'deregistration))))
      (_ nil))))

(defun cardano-tx--delegation-cert (cert)
  "Return delegation certificate file or create it if needed from object CERT."
  (when-let ((conf (cardano-tx-get-in cert 'delegation)))
    (cardano-tx-address-delegation-certificate
     (cardano-tx-get-in conf 'pool)
     (or
      (cardano-tx-get-in conf 'vkey-file)
      (cadar (cardano-tx-db-stake-keys))))))

(defun cardano-tx--build-instructions (input-data)
  "Build a transaction from INPUT-DATA."
  (let* ((mint-rows (cardano-tx--mint-rows (cardano-tx-get-in input-data 'mint)))
         (policy-name-replacer (cardano-tx--replace-mint-asset-names mint-rows))
         (tx-outs (cardano-tx-get-in input-data 'outputs))
         (build (seq-some (lambda (tx-out) (cardano-tx-get-in tx-out 'change)) tx-outs)))
    (mapc #'cardano-tx-save-datum tx-outs)
    (list "transaction" (if build "build" "build-raw") "--alonzo-era"
          (->> 'inputs
               (cardano-tx-get-in input-data)
               (mapcar #'cardano-tx--in-args))
          (->> tx-outs
               (--map (cardano-tx--out-args
                       (cardano-tx-alist-key-string it policy-name-replacer))))
          (cardano-tx--mints mint-rows)
          (cardano-tx--validity-interval input-data)
          (-some->> 'validity-interval
            (cardano-tx-get-in input-data)
            (cardano-tx--validity-interval))
          (-some--> 'metadata
            (cardano-tx-get-in input-data it)
            (cardano-tx-alist-key-string it policy-name-replacer)
            cardano-tx--metadata-args)
          (cardano-tx--plutus-args input-data)
          (when (and build (cardano-tx-get-in input-data 'witness))
            (->> (cardano-tx-witnesses input-data)
                 length number-to-string
                 (list "--witness-override")))
          (-some->> 'certificates
            (cardano-tx-get-in input-data)
            (cardano-tx--certificates))
          (-some->> (cardano-tx-get-in input-data 'withdrawals)
            (cardano-tx--withdrawals))
          (unless build  (list "--fee" (number-to-string (or (cardano-tx-get-in input-data 'fee) 0)))))))

(defun cardano-tx--build (input-data)
  "Build a transaction from INPUT-DATA."
  (let ((tx-file (make-temp-file "cardano-tx-")))
    (->> (append
          (cardano-tx--build-instructions input-data)
          (list "--out-file" tx-file))
         (flatten-tree)
         (apply #'cardano-tx-cli))
    tx-file))

(defun cardano-tx-view-or-hash (tx-file &optional hash)
  "Return transaction preview or HASH if set for TX-FILE."
  (cardano-tx-cli "transaction" (if hash "txid" "view")
                  (if (string-suffix-p ".signed" tx-file)
                      "--tx-file"
                    "--tx-body-file")
                  tx-file))

(defun cardano-tx-submit (tx-file)
  "Submit transaction on TX-FILE."
  (message "%s\nTxId: %s. Copied to kill-ring"
           (cardano-tx-cli "transaction" "submit" "--tx-file" tx-file)
           (kill-new (cardano-tx-view-or-hash tx-file t)))
  (cardano-tx-db-utxo-reset))

(defun cardano-tx--input-buffer ()
  "Parse the active transaction buffer into an alist."
  (yaml-parse-string (buffer-substring-no-properties (point-min) (point-max))
                     :object-type 'alist :object-key-type 'string))

(defun cardano-tx-available-balance (input-data)
  "Calculate and save as yaml into kill ring the available balance from INPUT-DATA."
  (interactive
   (list (yaml-parse-string (buffer-substring-no-properties (point-min) (point-max))
                            :object-type 'alist :object-key-type 'string
                            :null-object nil)))
  (let ((fee (list (cons "lovelace" (cardano-tx-get-in input-data 'fee))))
        (spent-value
         (mapcar (lambda (tx-out) (cardano-tx-assets-hexify (cardano-tx-get-in tx-out 'amount)))
                 (cardano-tx-get-in input-data 'outputs)))
        (tx-ins (cl-map 'vector (lambda (input) (cardano-tx-get-in input 'utxo))
                        (cardano-tx-get-in input-data 'inputs)))
        (mints (--map (cardano-tx-assets-hexify (list (cons (car it) (cardano-tx-get-in (cdr it) 'assets))))
                      (cardano-tx-get-in input-data 'mint)))
        (withdrawals (--map (cardano-tx-get-in it 'amount)
                            (cardano-tx-get-in input-data 'withdrawals))))
    (thread-last
      (append mints
              withdrawals
              (mapcar (-lambda ((_ _ lovelaces assets)) (cl-acons "lovelace" lovelaces assets))
                      (cardano-tx-db-utxo-info tx-ins))
              (list (--reduce-from (cardano-tx-assets-merge-alists #'- acc it) nil (cons fee spent-value))))

      (--reduce (cardano-tx-assets-merge-alists #'+ acc it))
      cardano-tx-assets-format-tokens
      (message)
      (replace-regexp-in-string "^" "      ")
      kill-new)))

(defun cardano-tx-edit-finish (preview)
  "Process buffer into a transaction, sign it and open PREVIEW."
  (interactive "P")
  (if-let ((input-data (cardano-tx--input-buffer))
           (tx-file (cardano-tx--build input-data)))
      (if preview
          (cardano-tx-review-before-submit tx-file (current-buffer))
        (thread-last (cardano-tx-witnesses input-data)
                     (cardano-tx-sign tx-file)
                     cardano-tx-submit)
        (kill-buffer))
    (error "Something is wrong.  Cannot parse the file")))

(defun cardano-tx-preview (tx-file)
  "Open buffer that previews transaction TX-FILE as displayed by `cardano-cli'."
  (interactive (list (read-file-name "Select transaction file: ")))
  (let ((buffer (get-buffer-create "*Cardano Preview tx*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# This is the transaction preview\n")
      (insert "# txid: " (cardano-tx-view-or-hash tx-file t) "\n\n")
      (insert (cardano-tx-view-or-hash tx-file))
      (cardano-tx-mode))
    (switch-to-buffer buffer)))

(defun cardano-tx-review-before-submit (tx-file originating-buffer)
  "Review transaction as displayed by `cardano-cli' for TX-FILE.

Set ORIGINATING-BUFFER as local variable."
  (with-current-buffer (cardano-tx-preview tx-file)
    (setq-local cardano-tx--buffer originating-buffer))
  (message "Press %s to send the transaction."
           (substitute-command-keys "\\[cardano-tx-send-from-preview]")))

(defun cardano-tx-send-from-preview ()
  "Send the transaction in buffer and kill corresponding transaction buffer."
  (interactive)
  (if cardano-tx--buffer
      (progn
        (with-current-buffer cardano-tx--buffer
          (cardano-tx-edit-finish nil))
        (kill-buffer))
    (message "This is not a transaction preview buffer.")))

(defun cardano-tx-new-script ()
  "Create a new native script."
  (interactive)
  (with-current-buffer (generate-new-buffer "*Cardano Native Script*")
    (yaml-mode)
    (yas-minor-mode-on)
    (yas-load-directory cardano-tx-snippet-dir)
    (switch-to-buffer (current-buffer))
    (readable-numbers-mode)
    (yas-expand-snippet (yas-lookup-snippet "native script"))
    (local-set-key "\C-c\C-c"
                   (lambda ()
                     (interactive)
                     (let* ((file-name
                             (cardano-tx-save-native-script (cardano-tx--input-buffer)))
                            (new-name (expand-file-name (file-name-nondirectory file-name)
                                                        cardano-tx-db-keyring-dir)))
                       (rename-file file-name new-name 'overwrite)
                       (cardano-tx-db-load-files (list new-name)))
                     (kill-buffer)))))

(defun cardano-tx-new ()
  "Open an editor to create a new transaction."
  (interactive)
  (let ((buffer (generate-new-buffer "*Cardano tx*")))
    (switch-to-buffer buffer)
    (insert "# -*- mode: cardano-tx; -*-\n\n")
    (cardano-tx-mode)
    (yas-expand-snippet (yas-lookup-snippet 'spend)))
  (message "Press %s to build and send transaction.\n With prefix to build and preview."
           (substitute-command-keys "\\[cardano-tx-edit-finish]")))

(defvar cardano-tx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'cardano-tx-edit-finish)
    (define-key map (kbd "C-c C-s") #'cardano-tx-send-from-preview)
    map)
  "Key map for `cardano-tx-mode'.")

(define-derived-mode cardano-tx-mode yaml-mode "cardano-tx"
  "Edit a transaction through a yaml representation."
  (readable-numbers-mode)
  (yas-minor-mode-on)
  (yas-load-directory cardano-tx-snippet-dir)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'cardano-tx-eldoc-documentation-function))

(defun cardano-tx-eldoc-documentation-function ()
  "Return the eldoc description of address at point."
  (pcase (thing-at-point 'symbol)
    ((pred null) nil)
    ((and (rx bol (or "addr" "stake") (opt "_test") "1") sym)
     (cardano-tx-address-decode sym))
    ((and (rx bol (= 64 hex)  "#" (+ digit) eol) sym)
     (-> (substring-no-properties sym) (vector)
         (cardano-tx-db-utxo-info) (car) (cardano-tx--utxo-contents)))))

(provide 'cardano-tx)
;;; cardano-tx.el ends here
