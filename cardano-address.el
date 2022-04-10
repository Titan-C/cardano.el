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
(require 'cardano-utils)
(require 'cardano-cli)
(require 'cardano-log)

(defgroup cardano-address nil
  "Address functionalities."
  :group 'cardano)

(defcustom cardano-address-keyring-dir "~/cardano-wallet-keys"
  "Folder where to store all the key files and addresses under management."
  :type 'directory)

(defvar cardano-address--list nil
  "List all available wallet addresses in keyring.")

(defun cardano-address-staking-key-file ()
  "Return a staking key compatible file by name."
  (car (directory-files cardano-address-keyring-dir t
                        (rx (or "_2_0" "stake") ".vkey" eol))))

(defun cardano-address--list ()
  "Return the list of all managed addresses in keyring."
  (if cardano-address--list
      cardano-address--list
    (cardano-address-populate)))

(defun cardano-address-named ()
  "Wallet list for easier selection."
  (mapcar (-lambda ((addr . path))
            (cons (concat (cardano-address--short addr)
                          (concat " # " (if (string-suffix-p "vkey" path)
                                            (file-name-base path)
                                          path)))
                  addr))
          (cardano-address--list)))

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
                      (directory-files cardano-address-keyring-dir t "\\.vkey$"))))
    (mapc
     (lambda (name)
       (if (member name keys)
           (cardano-log 'warn "Skip creating %S key pair, because it already exists." name)
         (cardano-address-new-key name)
         (cardano-log 'info "Created new key pair: %S" name)))
     names))
  (cardano-address-populate)
  (message "Keys created"))

(defun cardano-address-populate ()
  "Populate the cardano-address--list with all keys combined with STAKE-KEY."
  (interactive)
  (let ((stake-key (-some--> (cardano-address-staking-key-file)
                     (when (file-exists-p it) it))))
    (thread-last
      (directory-files cardano-address-keyring-dir t "\\.vkey$")
      (cl-remove-if (lambda (path) (string= stake-key path)))
      (mapcar (lambda (key)
                (cons (cardano-address-payment key stake-key)
                      key)))
      (setq cardano-address--list))))

(defun cardano-address-payment (vkey-file &optional stake-key)
  "Calculate payment address for VKEY-FILE optionally using STAKE-KEY for rewards."
  (apply #'cardano-cli
         "address" "build"
         "--payment-verification-key-file" vkey-file
         (when stake-key
           (list "--stake-verification-key-file" stake-key))))

(defun cardano-address-staking (vkey-file)
  "Construct staking address for key under VKEY-FILE."
  (cardano-cli "stake-address" "build"
               "--stake-verification-key-file" vkey-file))

(defun cardano-address-from-script (filename &optional stake-key-file)
  "Calculate the address of a script residing on FILENAME.
Optionally with the STAKE-KEY-FILE."
  (interactive
   (list (read-file-name "Select Plutus script file: ")))
  (kill-new
   (apply #'cardano-cli
          "address" "build"
          "--payment-script-file" (expand-file-name filename)
          (when stake-key-file
            (list "--stake-verification-key-file" stake-key-file)))))

(defun cardano-address-stake-registration-cert (vkey-file &optional leave)
  "Write stake address registration certificate from VKEY-FILE.
If LEAVE deregister."
  (let* ((action (pcase leave
                   ((or :false (pred null)) "registration")
                   (_ "deregistration")))
         (stake-registration-cert-file (concat vkey-file "." action ".cert")))
    (cardano-cli "stake-address" (concat action "-certificate")
                 "--stake-verification-key-file" vkey-file
                 "--out-file" stake-registration-cert-file)
    stake-registration-cert-file))

(defun cardano-address-delegation-certificate (pool-id &optional stake-vkey)
  "Create delegation certificate for POOL-ID.
Optionally define the STAKE-VKEY file."
  (let ((delegation-cert-file (make-temp-file "delegation" nil ".cert")))
    (cardano-cli "stake-address" "delegation-certificate"
                 "--stake-verification-key-file"
                 (or stake-vkey (cardano-address-staking-key-file))
                 "--stake-pool-id" pool-id
                 "--out-file" delegation-cert-file)
    delegation-cert-file))

(defun cardano-address--short (address)
  "Shorten ADDRESS for user display."
  (replace-regexp-in-string
   (rx (group "addr" (optional "_test") "1" (= 6 any)) (1+ any)
       (group (= 6 any)) eol)
   "\\1...\\2" address))

(defun cardano-address-pick ()
  "Let the user select an address from currently managed ones."
  (interactive)
  (let ((all-addr (cardano-address-named)))
    (-> (completing-read "Select an address: " all-addr)
        (assoc all-addr)
        cdr kill-new)))

(defun cardano-address-key-hash (vkey-file)
  "Get the key hash out of the VKEY-FILE."
  (interactive
   (list (read-file-name "Select verification key file: " cardano-address-keyring-dir
                         nil nil nil (lambda (n) (string-suffix-p ".vkey" n)))))
  (kill-new
   (concat
    (if (string= (cardano-address-staking-key-file) vkey-file)
        (cardano-cli "stake-address" "key-hash" "--stake-verification-key-file" vkey-file)
      (cardano-cli "address" "key-hash" "--payment-verification-key-file" vkey-file))
    " # " (file-name-base vkey-file ))))

(defun cardano-address-decode (address)
  "Decode ADDRESS string into its representation."
  (-let (((prefix (key . bt)) (bech32-decode address)))
    (-> (list
         (propertize (if (string-suffix-p "test" prefix)
                         "TestNet" "MainNet")
                     'face 'font-lock-warning-face)
         (propertize (pcase (ash key -4)
                       ((or #b0000 #b0110) "PubKeyHash")
                       ((or #b0001 #b0111) "ScriptHash")
                       (#b1110 "StakingKey")
                       (#b1111 "StakingScript")
                       (else (format "Else%s" else)))
                     'face 'font-lock-keyword-face)
         (cbor-string->hexstring (concat (seq-subseq bt 0 28)))
         (when (and (< (ash key -4) 4) (not (null (seq-subseq bt 28))))
           (propertize "StakingCredential" 'face 'font-lock-keyword-face))
         (cbor-string->hexstring (concat (seq-subseq bt 28))))
        (string-join " ")
        string-trim)))

(defun cardano-address-build (spend-type spend-hash reward-type reward-hash &optional network-id)
  "Build a bech32 Shelley address.

SPEND-TYPE and REWARD-TYPE are either nil, keyhash, script.
SPEND-HASH and REWARD-HASH are unichar strings.
NETWORK-ID is an int < 32. Defaults to 0 testnet, 1 is used for mainnet."
  (let ((type-flag (pcase (list spend-type reward-type)
                     ('(keyhash keyhash) #b0000)
                     ('(script  keyhash) #b0001)
                     ('(keyhash script)  #b0010)
                     ('(script  script)  #b0011)
                     ('(keyhash nil)     #b0110)  ;; Enterprise
                     ('(script  nil)     #b0111)  ;; Enterprise
                     ('(nil     keyhash) #b1110)  ;; Reward account
                     ('(nil     script)  #b1111)  ;; Reward account
                     (_ (error "Unsuported address type %s %s" spend-type reward-type))))
        (network-id (or network-id 0)))
    (bech32-encode (concat (if (= (ash type-flag -1) #b111) "stake" "addr")
                           (when (= network-id 0) "_test"))
                   (append (list (logior (ash type-flag 4) network-id))
                           spend-hash
                           reward-hash
                           nil))))

;;; HD wallets - wrapping cardano-addresses cli

(defcustom cardano-address-command (executable-find "cardano-address")
  "Which `cardano-address' binary to use."
  :type 'file
  :group 'cardano)

(defun cardano-address-path->str (key-path &optional separator)
  "KEY-PATH is list of path symbols on numbers.
Return joined string by SEPARATOR, defaults to `/'."
  (replace-regexp-in-string
   (or "h" "'") "H"
   (mapconcat #'prin1-to-string key-path (or separator "/"))))

(defun cardano-address--validate-hd-path (path)
  "Validate HD derivation PATH."
  (let ((split-path
         (--> (cond
               ((consp path) (cardano-address-path->str path))
               ((stringp path) path)
               (t ""))
              (split-string it (rx (or whitespace "/")))
              (cl-remove-if-not #'cardano-utils-nw-p it))))
    (when (--every (string-match-p (rx bol (1+ digit) (optional "H") eol) it) split-path)
      (mapcar
       (lambda (entry)
         (if (string-suffix-p "H" entry)
             (intern entry)
           (string-to-number entry)))
       split-path))))

(defun cardano-address-gen-recovery-phrase (size)
  "Create the recovery phrase of SIZE words for HD wallet.
Save it unencrypted on `cardano-address-keyring-dir'."
  (interactive
   (list
    (completing-read "How long shal the recovery phrase be? "
                     (mapcar #'number-to-string (number-sequence 9 24 3))
                     nil t)))
  (let ((phrase-file (expand-file-name "phrase.prv" cardano-address-keyring-dir)))
    (if (file-exists-p phrase-file)
        (cardano-log 'warn "There is already a recovery phrase file on your keyring")
      (with-temp-file phrase-file
          (cardano-cli-reply
           (call-process cardano-address-command nil (current-buffer) nil
                         "recovery-phrase" "generate"
                         "--size" (if (numberp size) (number-to-string size) size))))
      (set-file-modes phrase-file #o600))))

(defun cardano-address-piped (&rest args)
  "Call cardano-address with ARGS transform `current-buffer' from input to output."
  (cardano-log 'debug "%s %s" cardano-address-command
               (mapconcat #'prin1-to-string args " "))
  (apply #'call-process-region (point-min) (point-max)
         cardano-address-command
         t t nil
         args))

(defun cardano-address-master-key-from-phrase ()
  "Recovery word phrase on buffer to master key."
  (cardano-address-piped "key" "from-recovery-phrase" "Shelley"))

(defun cardano-address-derive-child (path)
  "Piped from buffer derive PATH from extended key."
  (cardano-address-piped "key" "child"
                         (if (stringp path)
                             path
                           (cardano-address-path->str path))))

(defun cardano-address-public-key (&optional without-chain-code)
  "Piped from buffer extract the public key optionally WITHOUT-CHAIN-CODE."
  (cardano-address-piped "key" "public"
                         (if without-chain-code
                             "--without-chain-code"
                           "--with-chain-code")))

(defun cardano-address--json-priv-key (xpriv-key pub-key type-name)
  "JSON string for an extended XPRIV-KEY with PUB-KEY of TYPE-NAME."
  (json-encode
   (list :type (concat type-name
                       "ExtendedSigningKeyShelley_ed25519_bip32")
         :description ""
         :cborHex (-> (concat (seq-subseq xpriv-key 0 64)
                              pub-key
                              (seq-subseq xpriv-key 64))
                      cbor-string->hexstring
                      cbor<-elisp
                      cbor-string->hexstring))))

(defun cardano-address--json-pub-key (pub-key type-name)
  "JSON string for a PUB-KEY file of TYPE-NAME."
  (json-encode
   (list :type (concat type-name
                       "VerificationKeyShelley_ed25519")
         :description (concat type-name " Verification Key")
         :cborHex (-> pub-key cbor-string->hexstring
                      cbor<-elisp cbor-string->hexstring))))

(defun cardano-address-cli-keys (out-file-name)
  "Save `cardano-cli' key-pairs labeled by OUT-FILE-NAME.
From extended key in `current-buffer'."
  (goto-char (point-min))
  (let* ((stake-p (looking-at "stake"))
         (type-name (if stake-p "Stake" "Payment"))
         (xpriv-key
          (-> (buffer-string) bech32-decode cadr))
         (pub-key
          (progn (cardano-address-public-key t)
                 (-> (buffer-string) bech32-decode cadr))))
    ;; Write secret signing key
    (let ((out-file (concat out-file-name ".skey")))
      (-> (cardano-address--json-priv-key xpriv-key pub-key type-name)
          (f-write 'utf-8 out-file))
      (set-file-modes out-file #o600))
    ;; Write verification key
    (let ((out-file (concat out-file-name".vkey")))
      (-> (cardano-address--json-pub-key pub-key type-name)
          (f-write 'utf-8 out-file))
      (set-file-modes out-file #o600))))

(defun cardano-address-new-hd-key (path)
  "Create new HD key-pair at PATH from wallet recovery phrase."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "phrase.prv" cardano-address-keyring-dir))
    (cardano-address-master-key-from-phrase)
    (cardano-address-derive-child path)
    (cardano-address-cli-keys
     (expand-file-name (cardano-address-path->str path "_")
                       cardano-address-keyring-dir))))

(defun cardano-address-new-hd-key-files (path)
  "Generate the key pairs for HD derivation PATH."
  (interactive
   (list (read-string "Specify the derivation path: " "1852H/1815H/0H/")))
  (when-let ((path (cardano-address--validate-hd-path path))
             (named-path (cardano-address-path->str path "_")))
    (let ((keys (mapcar #'file-name-base
                        (directory-files cardano-address-keyring-dir t "\\.vkey$"))))
      (if (member named-path  keys)
          (cardano-log 'warn "Skip creating %S key pair, because it already exists." named-path)
        (cardano-address-new-hd-key path)
        (cardano-log 'info "Created new key pair: %S" named-path)))))

(provide 'cardano-address)
;;; cardano-address.el ends here
