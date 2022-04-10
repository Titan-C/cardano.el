;;; cardano-wallet.el --- Interact with cardano wallet -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://github.com/titan>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;;
;;; Commentary:
;;
;; Provide an interface to the cardano-wallet service
;;; Code:

(require 'url)
(require 'yaml)
(require 'dash)
(require 'yaml-mode)
(require 'numbers)
(require 'cardano-utils)
(require 'cardano-address)
(require 'cardano-tx)

;; Silence byte-compiler.
(defvar url-http-end-of-headers)

(defgroup cardano-wallet nil
  "Wallet service."
  :group 'cardano)

(defcustom cardano-wallet-url nil
  "Cardano wallet url with port."
 :type 'string)

(defvar-local cardano-wallet-tx--wallet nil
  "Hashtable containing active wallet information.
This is only available on TX preview buffers.")

(defun cardano-wallet (endpoint callback &optional method json-data)
  "Wallet API call to ENDPOINT.

CALLBACK function that takes a hash-table, which is the parsed JSON of response.
If JSON-DATA default to post unless METHOD is defined."
  (let ((url-request-method method)
        (url-request-extra-headers
         (when json-data '(("Content-Type" . "application/json"))))
        (url-request-data
         (-some-> json-data json-encode (encode-coding-string 'utf-8))))
    (url-retrieve
     (concat cardano-wallet-url "/v2/" endpoint)
     #'cardano-utils--parse-response
     `(,callback))))

(defun cardano-wallet-show (json)
  "Show whatever is the response JSON."
  (with-current-buffer (get-buffer-create "*Cardano wallet response*")
    (erase-buffer)
    (insert (yaml-encode json))
    (yaml-mode)
    (numbers-separator-mode)
    (display-buffer (current-buffer))))

(defun cardano-wallet-address-show (json)
  "Show addresses from JSON."
  (with-current-buffer (get-buffer-create "*Cardano Wallet addresses*")
    (tabulated-list-mode)
    (setq tabulated-list-format [("Path" 7 t)
                                 ("state" 5 t)
                                 ("Address" 0 t)]
          tabulated-list-entries
          (seq-map (lambda (res)
                     (let ((deriv (-> (cardano-utils-get-in res "derivation_path")
                                      (seq-drop 2)
                                      (string-join "/")))
                           (state (cardano-utils-get-in res "state"))
                           (address (cardano-utils-get-in res "id")))
                       (list deriv (vector
                                    (if (string= state "used")
                                        (propertize deriv 'face 'font-lock-keyword-face)
                                      deriv) state address))))
                   json))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(defun cardano-wallet-readable-number (amount)
  "Redably stringify AMOUNT."
  (let ((ref (number-to-string amount)))
    (if (> 4 (length ref))
        ref
      (let ((start (if (= 0 (string-match (rx (+ (= 3 digit)) eol) ref))
                       3 (match-beginning 0))))
        (string-join
         (cons
          (substring ref 0 start)
          (--map (substring ref it (+ it +3))
                 (number-sequence start (1- (match-end 0)) 3)))
         "_")))))

(defun cardano-wallet-sort-readable-number (column)
  "Generate a sort function for `tabulated-list' readable number at COLUMN."
  (cl-flet ((value (lambda (entry)
                     (thread-last
                       (aref (cadr entry) column)
                       (replace-regexp-in-string "_" "")
                       (string-to-number)))))
    (lambda (e1 e2) (> (value e1) (value e2)))))

(defun cardano-wallet-print-col (value)
  "Format VALUE for column."
  (pcase value
    ((pred integerp) (cardano-wallet-readable-number value))
    ((pred stringp) value)
    (n (format "%S" n))))

(defun cardano-wallet--balance-row (entry)
  "Extract values from ENTRY to display a balance row."
  (list (cardano-utils-get-in entry "balance" "total" "quantity")
        (cardano-utils-get-in entry "balance" "available" "quantity")
        (cardano-utils-get-in entry "balance" "reward" "quantity")
        (if (seq-empty-p (cardano-utils-get-in entry "assets" "total"))
            "NO" "YES")
        (cardano-utils-get-in entry "delegation" "active" "target")))

(defun cardano-wallet-balances-show (json)
  "Show balances table from JSON."
  (with-current-buffer (get-buffer-create "*balances*")
    (tabulated-list-mode)
    (cl-flet ((sorter (lambda (col) (cardano-wallet-sort-readable-number col))))
      (setq tabulated-list-format `[("Name" 10 t)
                                    ("ID" 10 t)
                                    ("Total" 15 ,(sorter 2) :right-align t)
                                    ("Available" 15 ,(sorter 3) :right-align t)
                                    ("Reward" 13 ,(sorter 4) :right-align t)
                                    ("Assets" 6 t)
                                    ("Delegation" 0 t)]))
    (setq tabulated-list-entries
          (seq-map
           (lambda (res)
             (let ((name (cardano-utils-get-in res "name"))
                   (id (cardano-utils-get-in res "id")))
               (list id (cl-map 'vector
                                #'cardano-wallet-print-col
                                (append (list name id)
                                        (cardano-wallet--balance-row res))))))
           json))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer (current-buffer))))


(defun cardano-wallet-balances ()
  "Pop a buffer with all watched wallets."
  (interactive)
  (cardano-wallet
   "wallets"
   #'cardano-wallet-balances-show))

(defun cardano-wallet-helm-pick ()
  "Helm actions on available wallets."
  (interactive)
  (cardano-wallet
   "wallets"
   (lambda (json)
     (helm
      :prompt "Wallet: "
      :sources (helm-build-sync-source "wallet"
                 :candidates (--map (cons (cardano-utils-get-in it "name") it)
                                    json)
                 :action (helm-make-actions
                          "Payment" #'cardano-wallet-tx-new
                          "Get addresses" #'cardano-wallet-addresses
                          "Get transaction log" #'cardano-wallet-tx-log
                          "Describe wallet" #'cardano-wallet-show
                          "Delete wallet" #'cardano-wallet-delete))))))

(defun cardano-wallet-addresses (wallet)
  "Pop buffer with addresses for WALLET."
  (cardano-wallet
   (format "wallets/%s/addresses" (cardano-utils-get-in wallet "id"))
   #'cardano-wallet-address-show))

(defun cardano-wallet-tx-log (wallet)
  "Pop transaction log for WALLET."
  (cardano-wallet
   (format "wallets/%s/transactions"
           (cardano-utils-get-in wallet "id"))
   (lambda (json) (cardano-wallet-tx-log-show wallet json))))

(defun cardano-wallet-tx (wallet-id tx-hash)
  "Show TX-HASH from WALLET-ID."
  (interactive
   (list
    (cardano-utils-get-in cardano-wallet-tx--wallet "id")
    (aref (tabulated-list-get-entry) 1)))
  (cardano-wallet
   (format "wallets/%s/transactions/%s" wallet-id tx-hash)
   #'cardano-wallet-show))

(defun cardano-wallet-create (name mnemonic passphrase)
  "Register a new wallet under NAME from MNEMONIC secured with PASSPHRASE."
  (interactive
   (list
    (read-from-minibuffer "Name your wallet: ")
    (split-string (f-read
                   (read-file-name "File with your seed phrase" cardano-address-keyring-dir)))
    (read-passwd "Password for your wallet: " t)))
  (cardano-wallet
   "wallets"
   #'cardano-wallet-show
   "POST"
   (list :name name
         :mnemonic_sentence mnemonic
         :passphrase passphrase)))

(defun cardano-wallet-delete (wallet)
  "Delete WALLET."
  (let ((wallet-id (cardano-utils-get-in wallet "id")))
    (cardano-wallet
     (concat "wallets/" wallet-id)
     (lambda (_x) (message "Wallet %s deleted." wallet-id))
     "DELETE")))

(defun cardano-wallet-tx-log-show (wallet json)
  "Show balances table in WALLET from JSON."
  (with-current-buffer (get-buffer-create "*transactions*")
    (tabulated-list-mode)
    (setq-local cardano-wallet-tx--wallet wallet)
    (cl-flet ((sorter (lambda (col) (cardano-wallet-sort-readable-number col))))
      (setq tabulated-list-format `[("Date" 20 t)
                                    ("Hash" 10 t)
                                    ("Fee" 13 ,(sorter 2) :right-align t)
                                    ("Total" 13 ,(sorter 3) :right-align t)]))
    (setq tabulated-list-entries
          (seq-map
           (lambda (res)
             (let ((date (cardano-utils-get-in res "inserted_at" "time"))
                   (id (cardano-utils-get-in res "id"))
                   (net-amount (cardano-utils-get-in res "amount" "quantity")))
               (list id
                     (cl-map 'vector
                             #'cardano-wallet-print-col
                             (list date id
                                   (cardano-utils-get-in res "fee" "quantity")
                                   (if (string= "incoming" (cardano-utils-get-in res "direction"))
                                       net-amount
                                     (propertize (cardano-wallet-print-col net-amount)
                                                 'face 'font-lock-keyword-face)))))))
           json))
    (tabulated-list-init-header)
    (tabulated-list-print)
    (display-buffer (current-buffer))))

(defun cardano-wallet-tx-finish ()
  "Process buffer into a transaction, sign it and open PREVIEW."
  (interactive)
  (let* ((input-data (cardano-tx--input-buffer))
         (outputs (cardano-utils-get-in input-data 'outputs))
         (tx-buffer (current-buffer)))
    (cardano-wallet
     (concat "wallets/"
             (cardano-utils-get-in cardano-wallet-tx--wallet "id")
             "/transactions")
     (lambda (json) (cardano-wallet-show json) (kill-buffer tx-buffer))
     "POST"
     `(:payments ,(cl-map 'vector #'cardano-wallet-payment outputs)
       :passphrase ,(read-passwd "Password to unlock your wallet: ")))))

(defun cardano-wallet-payment (tx-out)
  "Build payment object from TX-OUT."
  (if-let ((target-addr (cardano-utils-nw-p
                         (cardano-utils-get-in tx-out 'address)))
           (amount  (cardano-utils-get-in tx-out 'amount)))
      `(:address ,target-addr
        :amount (:quantity ,(cardano-utils-get-in tx-out 'amount 'lovelace)
                 :unit "lovelace")
        :assets
        ,(->>
          (cardano-assets-flatten amount)
          (cl-remove-if #'numberp)
          (cl-map 'vector
                  (-lambda ((amount policy tokenname))
                    (list :policy_id policy
                          :asset_name (cbor-string->hexstring tokenname)
                          :quantity amount)))))))

(defun cardano-wallet-tx-new (wallet)
  "Open an editor to create a new transaction for WALLET."
  (interactive)
  (with-current-buffer (generate-new-buffer "*cardano tx*")
    (insert "# -*- mode: cardano-tx; -*-\n\n")
    (cardano-tx-mode)
    (setq-local cardano-wallet-tx--wallet wallet)
    (yas-expand-snippet (yas-lookup-snippet 'wallet-spend))
    (message "Press %s to build and send transaction."
             (substitute-command-keys "\\[cardano-wallet-tx-finish]"))
    (switch-to-buffer (current-buffer))))

(defun cardano-wallet-assets-pick ()
  "Load assets available on wallet to `kill-ring'."
  (interactive)
  (->> (cardano-utils-get-in cardano-wallet-tx--wallet "assets" "total")
       (--map
        (concat "      " ;; ident
         (cardano-utils-get-in it "policy_id")
         ":\n        "
         (let ((assetname (cbor-hexstring->ascii
                           (cardano-utils-get-in it "asset_name"))))
           (if (cardano-utils-nw-p assetname) assetname "\"\"")) ": "
         (number-to-string
          (cardano-utils-get-in it "quantity")) "\n"))
       (cardano-utils-pick "Assets")
       (string-join)
       (kill-new)))

(provide 'cardano-wallet)
;;; cardano-wallet.el ends here
