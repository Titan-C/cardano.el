;;; cardano-address.el --- Manipulate cardano addresses -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "25.1"))
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
;; Wrapping around cardano-cli to manage addresses
;;
;;; Code:

(require 'f)
(require 'bech32)
(require 'cbor)
(require 'subr-x)
(require 'cardano-cli)
(require 'logger)

(defgroup cardano-address nil
  "Address functionalities"
  :group 'cardano)

(defcustom cardano-address-keyring-dir "~/cardano-wallet-keys"
  "Folder where to store all the key files and addresses under management."
  :type 'directory)

(defvar cardano-address--list nil
  "List all available wallet addresses in keyring.")

(defun cardano-address--list ()
  "Return the list of all managed addresses in keyring."
  (if cardano-address--list
      cardano-address--list
    (setq cardano-address--list
          (mapcar #'cardano-address-file-pair
                  (directory-files cardano-address-keyring-dir t "\\.addr$")))))

(defun cardano-address-file-pair (filename)
  "Read FILENAME contents and return cons of it and FILENAME."
  (cons (f-read filename) filename))

(defun cardano-address-named ()
  "Wallet list for easier selection."
  (mapcar (-lambda ((address . path))
            (cons (file-name-base path) address))
          (cardano-address--list)) )

(defun cardano-address-new-key (name &optional stake)
  "Create new payment keys under NAME.
If STAKE is non-nil generate stake key."
  (let* ((prefix (expand-file-name name cardano-address-keyring-dir))
         (type (if stake "stake-address" "address"))
         (v-file (concat prefix ".vkey"))
         (s-file (concat prefix ".skey")))
    (cardano-cli
     type "key-gen"
     "--verification-key-file" v-file
     "--signing-key-file" s-file)))

(defun cardano-address-new-key-files (&rest names)
  "Generate the key pairs for each one of NAMES.
Files are located in keyring dir together with matching address files."
  (interactive
   (split-string
    (read-string "How do you want to name your keys(separate with space for many): ")))
  (let ((keys (mapcar #'file-name-base
                      (directory-files cardano-address-keyring-dir t "\\.vkey$")))
        (logger-buffer-name "*cardano-log*"))
    (mapc
     (lambda (name)
       (if (member name keys)
           (logger 'warn "Skip creating %S key pair, because it already exists." name)
         (cardano-address-new-key name)
         (cardano-address-get-addresses name)
         (logger 'info "Created new key pair: %S" name)))
     names))
  (message "Keys created"))

(defun cardano-address-payment (name &optional no-stake)
  "Create payment address under NAME.
If NO-STAKE is non-nil omit stake key in address."
  (let* ((prefix (expand-file-name name cardano-address-keyring-dir))
         (v-file (concat prefix ".vkey"))
         (address-file (concat prefix (if no-stake "-enterprise" "") ".addr")))
    (apply #'cardano-cli
           "address" "build"
           "--payment-verification-key-file" v-file
           "--out-file" address-file
           (unless no-stake
             (list "--stake-verification-key-file"
                   (expand-file-name "stake.vkey" cardano-address-keyring-dir))))))

(defun cardano-address-staking (name)
  "Construct staking address for key under NAME."
  (let* ((prefix (expand-file-name name cardano-address-keyring-dir))
         (v-file (concat prefix ".vkey"))
         (stake-addr-file (concat prefix ".stake-addr")))
    (cardano-cli "stake-address" "build"
                 "--stake-verification-key-file" v-file
                 "--out-file" stake-addr-file)))

(defun cardano-address-from-script (filename)
  "Calculate the address of a script residing on FILENAME."
  (interactive
   (list (read-file-name "Select Plutus script file: ")))
  (kill-new
   (cardano-cli "address" "build"
                "--payment-script-file" (expand-file-name filename))))

(defun cardano-address-get-addresses (name &optional no-stake)
  "Generate the address files for keys of NAME.
Include the wallet staking address unless NO-STAKE is non-nil."
  (setq cardano-address--list nil)
  (unless (file-exists-p (expand-file-name "stake.vkey" cardano-address-keyring-dir))
    (cardano-address-new-key "stake" t)
    (cardano-address-staking "stake"))
  (cardano-address-payment name no-stake))

(defun cardano-address-helm ()
  "Let the user select an address from currently managed ones."
  (interactive)
  (let* ((all-addr (mapcar #'file-name-base
                           (directory-files cardano-address-keyring-dir t "\\.addr$")))
         (name (completing-read "Select an address: "
                                all-addr)))
    (kill-new
     (concat (f-read (expand-file-name (concat name ".addr")
                                       cardano-address-keyring-dir))
             " # " name))))

(defun cardano-address-key-hash (vkey-file)
  "Get the key hash out of the VKEY-FILE."
  (interactive
   (list (read-file-name "Select verification key file: " cardano-address-keyring-dir
                         nil nil nil (lambda (n) (string-suffix-p ".vkey" n)))))
  (kill-new
   (concat
    (cardano-cli "address" "key-hash" "--payment-verification-key-file" vkey-file)
    " # " (file-name-base vkey-file ))))

(defun cardano-address-decode (address)
  "Decode ADDRESS string into its representation."
  (-let (((prefix (key . bt)) (bech32-decode address)))
    (-> (list
         (propertize (if (string-suffix-p "test" prefix)
                         "TestNet" "MainNet")
                     'face 'font-lock-warning-face)
         (propertize (pcase key
                       ((or 0 1) "PubKeyHash")  ;; with stake
                       ((or 96 97) "PubKeyHash") ;; enterprise
                       ((or 112 113) "PlutusScript"))
                     'face 'font-lock-keyword-face)
         (cbor-string->hexstring (concat (seq-subseq bt 0 28)))
         (when (and (memq key '(0 1)) (not (null (seq-subseq bt 28))))
           (propertize "StakingCredential" 'face 'font-lock-keyword-face))
         (cbor-string->hexstring (concat (seq-subseq bt 28))))
        (string-join " ")
        string-trim)))

(provide 'cardano-address)
;;; cardano-address.el ends here
