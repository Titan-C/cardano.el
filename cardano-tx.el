;;; cardano-tx.el --- Cardano transaction editor -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "27.1") (dash "2.19.0") (yasnippet "0.14.0") (yaml-mode "0.0.15") (yaml "0.1.0") (helm "3.6.2"))
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
(require 'numbers)
(require 'cardano-cli)
(require 'cardano-address)
(require 'cardano-assets)
(require 'cardano-utils)

(defconst cardano-tx-snippet-dir
  (concat (file-name-directory (or load-file-name buffer-file-name)) "snippets"))

(defvar cardano-tx--utxos-list nil
  "List of all UTXOs available on controlled addresses.")

(defun cardano-tx--utxos-list ()
  "Return the list of all managed UTXOs in keyring."
  (if cardano-tx--utxos-list
      cardano-tx--utxos-list
    (setq cardano-tx--utxos-list (cardano-tx-utxos (mapcar #'car (cardano-address--list))))))

(defvar-local cardano-tx--buffer nil
  "Buffer containing the transaction specification. This is only available on TX preview buffers.")

(defun cardano-tx-rewards (address)
  "Recover the rewards info sitting at ADDRESS."
  (interactive (list (read-string "Stake address: "
                                  (thing-at-point 'symbol))))
  (let ((rewards-file (make-temp-file "rewards-" nil ".json"))
        (json-key-type 'string))
    (cardano-cli "query" "stake-address-info" "--out-file" rewards-file
                 "--address" address)
    (when-let ((result
                (cardano-utils-get-in (json-read-file rewards-file) 0)))
      (when (called-interactively-p 'interactive)
        (cardano-cli-pretty-yaml-message result))
      (kill-new
       (number-to-string
        (cardano-utils-get-in result 'rewardAccountBalance))))))

(defun cardano-tx-utxos (addresses)
  "Recover the utxos sitting at ADDRESSES."
  (let ((utxos-file (make-temp-file "utxos-" nil ".json")))
    (apply #'cardano-cli "query" "utxo" "--out-file" utxos-file
           (--mapcat (list "--address" it) addresses))
    (let ((json-key-type 'string))
      (json-read-file utxos-file))))

(defun cardano-tx--utxo-contents (utxo)
  "Human readable contents of UTXO."
  (-> (list (-some->> "value" (cardano-utils-get-in utxo) cardano-assets-format-tokens)
            (-some->> "data" (cardano-utils-get-in utxo) (concat "\ndatumhash: ")))
      (string-join "\n")
      string-trim-right))

(defun cardano-tx--helm-candidate (utxo-info)
  "Prepare a helm candidate given the UTXO-INFO of balances."
  (cons
   (concat (car utxo-info) "\n" (cardano-tx--utxo-contents (cdr utxo-info)))
   utxo-info))

(defun cardano-tx-utxos-for-helm (utxos)
  "Prepare helm candidates from parsed JSON of UTXOS."
  (sort
   (mapcar #'cardano-tx--helm-candidate utxos)
   (lambda (a b)
     (string< (car a) (car b)))))

(defun cardano-tx-helm-utxos ()
  "Pick from wallet controlled utxos and put them on kill ring."
  (interactive)
  (kill-new
   (mapconcat #'car
              (cardano-utils-pick "Select UTXOS" (cardano-tx-utxos-for-helm (cardano-tx--utxos-list)))
              "\n  - utxo: ")))

(defun cardano-tx-witnesses (input-data)
  "Given the INPUT-DATA about to be spent.  Which wallets control them?
All the wallet address-file pairs in the keyring are tested."
  (let ((wallets (cardano-address--list))
        (utxos (cardano-tx--utxos-list))
        (utxos-used (cons (cardano-utils-get-in input-data 'collateral)
                          (--map (cardano-utils-get-in it 'utxo)
                                 (cardano-utils-get-in input-data 'inputs))))
        (witness-override (--map (expand-file-name (concat it ".skey") cardano-address-keyring-dir)
                                 (cardano-utils-get-in input-data 'witness))))

    (-> (--keep (-some->> (cardano-utils-get-in utxos it 'address)
                  (cardano-utils-get-in wallets)
                  (replace-regexp-in-string ".vkey$" ".skey"))
                utxos-used)
        (append witness-override)
        (delete-dups))))

(defun cardano-tx-sign (tx-file witness-keys)
  "Sign a transaction file TX-FILE with WITNESS-KEYS."
  (let ((signed-file (concat tx-file ".signed")))
    (apply #'cardano-cli (flatten-tree
                          (list "transaction" "sign"
                                "--tx-body-file" tx-file
                                (--map (list "--signing-key-file" it)
                                       witness-keys)
                                "--out-file" signed-file)))
    signed-file))

(defun cardano-tx-hash-script-data (datum)
  "Hash the DATUM."
  (cardano-cli "transaction" "hash-script-data" "--script-data-value" (json-encode datum)))

(defun cardano-tx-hash-script-data-file (datumfile)
  "Hash the DATUMFILE."
  (cardano-cli "transaction" "hash-script-data" "--script-data-file" datumfile))

(defun cardano-tx-datum-hash (tx-out)
  "Obtain the datum hash from TX-OUT.  Calculate from datum field if needed."
  (or (cardano-utils-nw-p (cardano-utils-get-in tx-out "datumhash"))
      (-some-> (cardano-utils-get-in tx-out "datum")
        cardano-utils-nw-p
        cardano-tx-hash-script-data)
      (-some-> (cardano-utils-get-in tx-out "datumfile")
        cardano-utils-nw-p
        expand-file-name
        cardano-tx-hash-script-data-file)))

(defun cardano-tx--value-amount (value)
  "Create sum string of output amount for VALUE."
  (-> (--map (if (numberp it)
                 (number-to-string it)
               (-let (((amount policy tokenname) it))
                 (format "%d %s.%s" amount policy (cbor-string->hexstring tokenname))))
             (cardano-assets-flatten value))
      (sort #'string>) ;; Sorting for lovelace amount first
      (string-join "+")))

(defun cardano-tx--format-addr-amount (output)
  "Format OUTPUT Lisp object with keys address & amount for CLI."
  (let ((address (cardano-utils-get-in output 'address))
        (amount  (cardano-utils-get-in output 'amount)))
    (concat address "+" (cardano-tx--value-amount amount))))

(defun cardano-tx--out-args (tx-out)
  "Generate the command line arguments for TX-OUT."
  (if-let ((target-addr (cardano-utils-nw-p (cardano-utils-get-in tx-out 'address))))
      (if (cardano-utils-get-in tx-out 'change)
          (list "--change-address" target-addr)
        (list "--tx-out" (cardano-tx--format-addr-amount tx-out)
              (-some->> (cardano-tx-datum-hash tx-out) (list "--tx-out-datum-hash"))))))

(defun cardano-tx--translate-mint-policy (mint-script)
  "Convert a MINT-SCRIPT object to JSON-file or pass the JSON-file."
  (if (and (stringp mint-script) (string-suffix-p ".script" mint-script))
      mint-script
    (let ((script (make-temp-file "mint-policy" nil )))
      (f-write (json-encode mint-script) 'utf-8 script)
      (let ((new-name (concat (file-name-directory script) (cardano-tx-policyid script) ".script")))
        (rename-file script new-name 'overwrite)
        new-name))))

(defun cardano-tx-policyid (mint-script-file)
  "Calculate the policy id for MINT-SCRIPT-FILE."
  (interactive (list (read-file-name "Which policy script: ")))
  (kill-new (cardano-cli "transaction" "policyid" "--script-file" mint-script-file)))

(defun cardano-tx--mint-rows (mints)
  "From the MINTS alist return rows of:

\(name policy-id json-policy-file assets\)"
  (mapcar (-lambda ((policy-name . conditions))
            (let ((policy-file (cardano-tx--translate-mint-policy (cardano-utils-get-in conditions 'policy))))
              (list policy-name
                    (cardano-tx-policyid policy-file)
                    policy-file
                    (cardano-utils-get-in conditions 'assets))))
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
  "Return function to replace minted assets variable names with the actual policy-id from the MINT-ROWS."
  (let ((replace-table (--map (cons (car it) (cadr it)) mint-rows)))
    (lambda (input)
      (alist-get input replace-table input nil #'string=))))

(defun cardano-tx--data-retrieve (data-type tx-in)
  "Obtain the DATA-TYPE either \"datum\" or \"redeemer\" for TX-IN."
  (or (-some->> data-type intern (cardano-utils-get-in tx-in) json-encode
                (list (format "--tx-in-%s-value" data-type)))
      (-some->> (concat data-type "file") intern (cardano-utils-get-in tx-in) expand-file-name
                (list (format "--tx-in-%s-file" data-type)))))

(defun cardano-tx--in-args (tx-in)
  "Generate the command line arguments for TX-IN."
  (list "--tx-in" (cardano-utils-get-in tx-in 'utxo)
        (-some--> (cardano-utils-get-in tx-in 'script-file) cardano-utils-nw-p expand-file-name
                 (list "--tx-in-script-file" it
                       (-> "datum"    (cardano-tx--data-retrieve tx-in))
                       (-> "redeemer" (cardano-tx--data-retrieve tx-in))))))

(defun cardano-tx--plutus-args (input-data)
  "Extra arguments when dealing with Plutus smart contacts, based on INPUT-DATA."
  (when-let ((tx-in-collateral (cardano-utils-get-in input-data 'collateral))
             (params-file (make-temp-file "cardano-params" nil ".json")))
    (cardano-cli "query" "protocol-parameters" "--out-file" params-file)
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
   (-some->> (cardano-utils-get-in validity-interval (concat "invalid-" it))
     number-to-string (list (concat "--invalid-" it)))
   '("before" "hereafter")))

(defun cardano-tx--withdrawals (withdrawals)
  "Instructions for WITHDRAWALS."
  (--map (list "--withdrawal" (cardano-tx--format-addr-amount it)) withdrawals))

(defun cardano-tx--certificates (certificates)
  "Instructions to upload CERTIFICATES."
  (--map
   (-some->> (or (cardano-utils-get-in it 'file)
                 (cardano-tx--registration-cert it)
                 (cardano-tx--delegation-cert it))
     (list "--certificate-file"))
   certificates))

(defun cardano-tx--registration-cert (cert)
  "Return registration certificate file or create it if needed from object CERT."
  (let ((default-stake-key (expand-file-name "stake.vkey" cardano-address-keyring-dir)))
    (pcase (cardano-utils-get-in cert 'registration)
      ((pred null) nil)
      (:null (cardano-address-stake-registration-cert default-stake-key))
      ((and conf (pred listp))
       (-> (or (cardano-utils-get-in conf 'vkey-file) default-stake-key)
           (cardano-address-stake-registration-cert
            (cardano-utils-get-in conf 'deregistration))))
      (_ nil))))

(defun cardano-tx--delegation-cert (cert)
  "Return delegation certificate file or create it if needed from object CERT."
  (when-let ((conf (cardano-utils-get-in cert 'delegation)))
    (cardano-address-delegation-certificate
     (cardano-utils-get-in conf 'pool)
     (cardano-utils-get-in conf 'vkey-file))))

(defun cardano-tx--build-instructions (input-data)
  "Build a transaction from INPUT-DATA."
  (let* ((mint-rows (cardano-tx--mint-rows (cardano-utils-get-in input-data 'mint)))
         (policy-name-replacer (cardano-tx--replace-mint-asset-names mint-rows)))
    (list "transaction" "build" "--alonzo-era"
          (->> 'inputs
               (cardano-utils-get-in input-data)
               (mapcar #'cardano-tx--in-args))
          (->> 'outputs
               (cardano-utils-get-in input-data)
               (--map (cardano-tx--out-args
                       (cardano-utils-alist-key-string it policy-name-replacer))))
          (cardano-tx--mints mint-rows)
          (cardano-tx--validity-interval input-data)
          (-some->> 'validity-interval
            (cardano-utils-get-in input-data)
            (cardano-tx--validity-interval))
          (-some--> 'metadata
            (cardano-utils-get-in input-data it)
            (cardano-utils-alist-key-string it policy-name-replacer)
            cardano-tx--metadata-args)
          (cardano-tx--plutus-args input-data)
          (when (cardano-utils-get-in input-data 'witness)
            (->> (cardano-tx-witnesses input-data)
                 length number-to-string
                 (list "--witness-override")))
          (-some->> 'certificates
            (cardano-utils-get-in input-data)
            (cardano-tx--certificates))
          (-some->> (cardano-utils-get-in input-data 'withdrawals)
            (cardano-tx--withdrawals)))))

(defun cardano-tx--build (input-data)
  "Build a transaction from INPUT-DATA."
  (let* ((tx-file (make-temp-file "cardano-tx-"))
         (inst (flatten-tree
                (append
                 (cardano-tx--build-instructions input-data)
                 (list "--out-file" tx-file)))))
    (apply #'cardano-cli inst)
    tx-file))

(defun cardano-tx-view-or-hash (tx-file &optional hash)
  "Return transaction preview or HASH if set for TX-FILE."
  (cardano-cli "transaction" (if hash "txid" "view")
               (if (string-suffix-p ".signed" tx-file)
                   "--tx-file"
                 "--tx-body-file")
               tx-file))

(defun cardano-tx-submit (tx-file)
  "Submit transaction on TX-FILE."
  (message "%s\nTxId: %s. Copied to kill-ring"
           (cardano-cli "transaction" "submit" "--tx-file" tx-file)
           (kill-new (cardano-tx-view-or-hash tx-file t)))
  (setq cardano-tx--utxos-list nil))

(defun cardano-tx--input-buffer ()
  "Parse the active transaction buffer into an alist."
  (yaml-parse-string (buffer-substring-no-properties (point-min) (point-max))
                     :object-type 'alist :object-key-type 'string))

(defun cardano-tx-available-balance ()
  "Calculate and save as yaml into kill ring the available balance from inputs."
  (interactive)
  (let* ((input-data (cardano-tx--input-buffer))
         (tx-ins (--map (cardano-utils-get-in it 'utxo) (cardano-utils-get-in input-data 'inputs))))
    (thread-last (--map (list (cons (car it) (cardano-utils-get-in (cdr it) 'assets)))
                        (cardano-utils-get-in input-data 'mint))
      (append (--map (cardano-utils-get-in (cardano-tx--utxos-list) it 'value) tx-ins))
      (--reduce (cardano-assets-merge-alists #'+ acc it))
      cardano-assets-format-tokens
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
  (let ((buffer (get-buffer-create "*cardano Preview tx*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "# This is the transaction preview\n")
      (insert "# txid: " (cardano-tx-view-or-hash tx-file t) "\n\n")
      (insert (cardano-tx-view-or-hash tx-file))
      (cardano-tx-mode))
    (switch-to-buffer buffer)))

(defun cardano-tx-review-before-submit (tx-file originating-buffer)
  "Review transaction from ORIGINATING-BUFFER as displayed by `cardano-cli' for TX-FILE."
  (with-current-buffer (cardano-tx-preview tx-file)
    (setq-local cardano-tx--buffer originating-buffer))
  (message "Press %s to send the transaction."
           (substitute-command-keys "\\[cardano-tx-send-from-preview]")))

(defun cardano-tx-send-from-preview ()
  "Send the transaction of this preview buffer and kill corresponding transaction buffers."
  (interactive)
  (if cardano-tx--buffer
      (progn
        (with-current-buffer cardano-tx--buffer
          (cardano-tx-edit-finish nil))
        (kill-buffer))
    (message "This is not a transaction preview buffer.")))

(defun cardano-tx-new ()
  "Open an editor to create a new transaction."
  (interactive)
  (let ((buffer (generate-new-buffer "*cardano tx*")))
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
  (numbers-separator-mode)
  (add-to-list 'yas-snippet-dirs cardano-tx-snippet-dir)
  (yas-load-directory cardano-tx-snippet-dir)
  (add-function :before-until (local 'eldoc-documentation-function)
                #'cardano-tx-eldoc-documentation-function))

(defun cardano-tx-eldoc-documentation-function ()
  "Return the eldoc description of address at point."
  (pcase (thing-at-point 'symbol)
    ((pred null) nil)
    ((and (rx bol (or "addr" "stake") (opt "_test") "1") sym)
     (cardano-address-decode sym))
    ((and (rx bol (= 64 hex)  "#" (+ digit) eol) sym)
     (cardano-tx--utxo-contents (cardano-utils-get-in (cardano-tx--utxos-list) sym)))))

(provide 'cardano-tx)
;;; cardano-tx.el ends here
