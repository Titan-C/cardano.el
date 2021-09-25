;;; cardano-address.el --- Manipulate cardano addresses -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
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
;; Wrapping around cardano-cli to manage addresses
;;
;;; Code:

(require 'f)
(require 'subr-x)
(require 'cardano-cli)

(defgroup cardano-address nil
  "Address functionalities"
  :group 'cardano)

(defcustom cardano-address-keyring-dir "~/cardano-wallet-keys"
  "Folder where to store all the key files and addresses under managment."
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

(defun cardano-address-new-key-files (&rest names)
  "Generate the key pairs for each one of NAMES.
Files are located in keyring dir together with matching address files."
  (interactive
   (thread-first
       (read-string "How do you want to name your keys(separate with space for many): ")
     split-string))
  (let ((keys (mapcar #'file-name-base
                      (directory-files cardano-address-keyring-dir t "\\.vkey$"))))
    (message
     (mapconcat
      (lambda (name)
        (if (member name keys)
            (format "%s: key pair already exists." name)
          (cardano-address-new-key name)
          (format "%s: New key pair created" name)))
      names "\n"))))
   "--signing-key-file" s-file)))

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

(defun cardano-address-insert ()
  "Let the user select an address from currently managed ones."
  (interactive)
  (let* ((all-addr (mapcar #'file-name-base
                           (directory-files cardano-address-keyring-dir t "\\.addr$")))
         (name (completing-read "Select an address: "
                                all-addr)))
    (insert (f-read (expand-file-name (concat name ".addr")
                                      cardano-address-keyring-dir))
            " # " name)))

(defun cardano-address-key-hash (vkey-file)
  "Get the key hash out of the VKEY-FILE."
  (cardano-cli "address" "key-hash" "--payment-verification-key-file" vkey-file))

(provide 'cardano-address)
;;; cardano-address.el ends here
