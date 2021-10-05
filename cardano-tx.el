;;; cardano-tx.el --- Cardano transaction editor -*- lexical-binding: t; -*-
;;
;;;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "27.1"))
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
(require 'yaml-mode)
(require 'json)
(require 'helm)
(require 'libyaml)
(require 'cardano-address)
(require 'cardano-cli)

(defvar cardano-tx--utxos-list nil
  "List of all UTXOs available on controlled addresses.")

(defun cardano-tx--utxos-list ()
  "Return the list of all managed UTXOs in keyring."
  (if cardano-tx--utxos-list
      cardano-tx--utxos-list
    (setq cardano-tx--utxos-list (cardano-tx-utxos (mapcar #'car (cardano-address--list))))))

(defvar-local cardano-tx--buffer nil
  "Buffer containing the transaction specification. This is only available on TX preview buffers.")

(defun cardano-tx-get-in (table &rest keys)
  "From nested hash-map TABLE get element in path of KEYS."
  (--reduce-from (when acc (gethash it acc)) table keys))

;; Shamelessly copied from ht.el
(defun cardano-tx-ht-map (function table)
  "Apply FUNCTION to each key-value pair of TABLE, return a list of results.
FUNCTION is called with two arguments, KEY and VALUE."
  (let (results)
    (maphash
     (lambda (key value)
       (push (funcall function key value) results))
     table)
    results))

(defun cardano-tx-nw-p (s)
  "Trimed not white-space string S."
  (when (stringp s)
    (let ((trimed (string-trim s)))
      (and (not (string-empty-p trimed)) trimed))))

(defun cardano-tx-pick (options-name candidates)
  "Simple multiple CANDIDATES picker of type OPTIONS-NAME."
  (helm
   :sources (helm-build-sync-source options-name
              :candidates candidates))
  (helm-marked-candidates))

(defun cardano-tx-utxos (addresses)
  "Recover the utxos sitting at ADDRESSES."
  (let ((utxos-file (make-temp-file "utxos-" nil ".json")))
    (apply #'cardano-cli "query" "utxo" "--out-file" utxos-file
           (--mapcat (list "--address" it) addresses))
    (let ((json-key-type 'string))
      (json-read-file utxos-file))))

(defun cardano-tx--helm-candidate (utxo-info)
  "Prepare a helm candidate given the UTXO-INFO of balances."
  (-let* (((utxo . info) utxo-info)
          (value (alist-get "value" info nil nil #'equal))
          (address (alist-get "address" info nil nil #'string=))
          (datumhash (alist-get "data" info nil nil #'string=)))
    (list
     (format "%s : %s"
             (replace-regexp-in-string "\\([[:xdigit:]]\\{8\\}\\).+\\([[:xdigit:]]\\{8\\}\\)#\\([[:digit:]]+\\)"
                                       "\\1..\\2#\\3" utxo)
             (if (numberp value)
                 (format "lovelace: %d" value)
               ;;starting on Mary
               (string-join (mapcar (-lambda ((asset . amount))
                                      (format "%s: %s" asset amount))
                                    value) ", ")))
     utxo
     value
     address
     datumhash)))

(defun cardano-tx-utxos-for-helm (utxos)
  "Prepare helm candidates from parsed JSON of UTXOS."
  (sort
   (mapcar #'cardano-tx--helm-candidate utxos)
   (lambda (a b)
     (string< (cadddr a) (cadddr b)))))

(defun cardano-tx-helm-insert-utxos ()
  "Pick from wallet controlled utxos and insert them on transaction editor buffer."
  (interactive)
  (insert
   (mapconcat #'car
              (cardano-tx-pick "Select UTXOS" (cardano-tx-utxos-for-helm (cardano-tx--utxos-list)))
              "\n  - utxo: ")))

(defun cardano-tx-witnesses (input-data)
  "Given the INPUT-DATA about to be spent. Which wallets control them?
All the wallet address-file pairs in the keyring are tested."
  (let ((wallets (cardano-address--list))
        (utxos (cardano-tx--utxos-list))
        (utxos-used (cons (gethash "collateral" input-data)
                          (--map (gethash "utxo" it) (gethash "inputs" input-data)))))
    (delete-dups (-keep (lambda (utxo)
                          (when-let* ((info (alist-get utxo utxos nil nil #'string=))
                                      (address (alist-get "address" info nil nil #'string=))
                                      (address-path (alist-get address wallets nil nil #'string=)))
                            (replace-regexp-in-string "\\(-enterprise\\)?\\.addr$" ".skey"
                                                      address-path)))
                        utxos-used))))

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

(defun cardano-tx-target-addr (tx-out)
  "Resolve the target address from the TX-OUT."
  (or (cardano-tx-nw-p (cardano-tx-get-in tx-out "address"))
      (when-let ((path (cardano-tx-nw-p (cardano-tx-get-in tx-out "address" "script-file"))))
        (cardano-address-from-script path))))

(defun cardano-tx-hash-script-data (datum)
  "Hash the DATUM."
  (cardano-cli "transaction" "hash-script-data" "--script-data-value" (json-encode datum)))

(defun cardano-tx-hash-script-data-file (datumfile)
  "Hash the DATUMFILE."
  (cardano-cli "transaction" "hash-script-data" "--script-data-file" datumfile))

(defun cardano-tx-datum-hash (tx-out)
  "Obtain the datum hash from TX-OUT. Calculate from datum field if needed."
  (or (cardano-tx-nw-p (cardano-tx-get-in tx-out "datumhash"))
      (when-let ((datum (cardano-tx-get-in tx-out "datum")))
        (cardano-tx-hash-script-data datum))
      (when-let ((datum (cardano-tx-get-in tx-out "datumfile")))
        (cardano-tx-hash-script-data-file (expand-file-name datum)))))

(defun cardano-tx--value-amount (value)
  "Create sum string of output amount for VALUE."
  (string-join
   (--map (if (numberp it)
              (number-to-string it)
            (apply #'format "%d %s.%s" it))
          (cardano-tx--multiasset value))
   "+"))

(defun cardano-tx--out-args (tx-out)
  "Generate the command line args for TX-OUT."
  (if-let ((target-addr (cardano-tx-target-addr tx-out)))
      (if (cardano-tx-get-in tx-out "change")
          (list "--change-address" target-addr)
        (list "--tx-out" (format "%s+%s"
                                 target-addr
                                 (cardano-tx--value-amount (cardano-tx-get-in tx-out "amount")))
              (when-let ((datumhash (cardano-tx-datum-hash tx-out)))
                (list "--tx-out-datum-hash" datumhash))))))

(defun cardano-tx--multiasset (value)
  "Return a list of all assets on the VALUE.
Lovelaces are numbers, other assets a 3-tuple(amount policyID tokenname)."
  (apply #'append
         (cardano-tx-ht-map
          (lambda (key val)
            (if (string= key "lovelace")
                (list val)
              (cardano-tx-ht-map
               (lambda (tokenname amount)
                 (list amount key tokenname))
               val)))
          value)))

(defun cardano-tx--data-retrieve (data-type tx-in)
  "Obtaing the DATA-TYPE either \"datum\" or \"redeemer\" for TX-IN."
  (or (when-let ((data (cardano-tx-get-in tx-in data-type)))
        (list (format "--tx-in-%s-value" data-type) (json-encode data)))
      (when-let ((data-file (cardano-tx-get-in tx-in (concat data-type "file"))))
        (list (format "--tx-in-%s-file" data-type) (expand-file-name data-file)))
      (error (format "Missing either %s or %sfile in transaction input" data-type data-type))))

(defun cardano-tx--in-args (tx-in)
  "Generate the command line args for TX-IN."
  (list "--tx-in" (gethash "utxo" tx-in)
        (when-let ((script-file (cardano-tx-get-in tx-in "script-file"))
                   (datum (cardano-tx--data-retrieve "datum" tx-in))
                   (redeemer (cardano-tx--data-retrieve "redeemer" tx-in)))
          (list "--tx-in-script-file" (expand-file-name script-file)
                datum
                redeemer))))

(defun cardano-tx--plutus-args (input-data)
  "Extra arguments when dealing with Plutus smart contacts, based on INPUT-DATA."
  (when-let ((tx-in-collateral (gethash "collateral" input-data))
             (params-file (make-temp-file "cardano-params" nil ".json")))
    (cardano-cli "query" "protocol-parameters" "--out-file" params-file)
    (list "--tx-in-collateral" tx-in-collateral
          "--protocol-params-file" params-file)))

(defun cardano-tx--build (input-data)
  "Build a transaction from INPUT-DATA."
  (let ((tx-ins
         (mapcar #'cardano-tx--in-args
                 (gethash "inputs" input-data)))
        (tx-outs
         (mapcar #'cardano-tx--out-args
                 (gethash "outputs" input-data)))
        (plutus (cardano-tx--plutus-args input-data))
        (tx-file (make-temp-file "cardano-tx-")))
    (apply #'cardano-cli (flatten-tree
                          (list "transaction" "build" "--alonzo-era"
                                tx-ins tx-outs plutus
                                "--out-file" tx-file)))
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

(defun cardano-tx-edit-finish (preview)
  "Process buffer into a transaction, sign it and open PREVIEW."
  (interactive "P")
  (if-let ((input-data (yaml-read-from-string (buffer-substring-no-properties (point-min) (point-max))))
           (tx-file (cardano-tx--build input-data)))
      (if preview
          (cardano-tx-review-before-submit tx-file (current-buffer))
        (thread-last (cardano-tx-witnesses input-data)
          (cardano-tx-sign tx-file)
          cardano-tx-submit)
        (kill-buffer))
    (error "Something is wrong. Cannot parse the file")))

(defun cardano-tx-preview (tx-file)
  "Open buffer that previews transaction TX-FILE as displayed by cardano-cli."
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
  "Review transaction from ORIGINATING-BUFFER as displayed by cardano-cli for TX-FILE."
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
    (with-current-buffer buffer
      (cardano-tx-mode)
      (insert "\
inputs:
  - utxo: # Select one or more utxos. For help use: cardano-tx-helm-insert-utxos

#   # If you are spendnig plutus scripts, you'll need to give more info
#   - utxo: # Select one or more utxos. For help use: cardano-tx-helm-insert-utxos
#     script-file: path
#     datum: json value
#     # datumfile: Typed datum values
#     redeemer: json value
#     # redeemerfile: Typed redeemer values

# # Collateral UTXO needed for plutus scripts
# collateral: # Select one. For help use: cardano-tx-helm-insert-utxos

outputs:
  - address: # Place here destination address. For help use: cardano-address-helm-insert
    # change: true # Place this flag if this shall be the change address ignores amount later
    # amount:
    #   lovelace: 0
    #   policyid....:
    #     tokenname1: 0
    #     tokenname2: 0
    # datum: For plutus scripts, conversion is direct
    # # datumfile: Typed datum values
    # # datumhash: For plutus scripts, provide the hash directly"))
    (switch-to-buffer buffer))
  (message "Press %s to build and send transaction.\n With prefix to build and preview."
           (substitute-command-keys "\\[cardano-tx-edit-finish]")))

(defvar cardano-tx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'cardano-tx-edit-finish)
    (define-key map (kbd "C-c C-s") #'cardano-tx-send-from-preview)
    map)
  "Keymap for `cardano-tx-mode'.")

(define-derived-mode cardano-tx-mode yaml-mode "cardano-tx"
  "Edit a transaction through a yaml representation.")

(provide 'cardano-tx)
;;; cardano-tx.el ends here
