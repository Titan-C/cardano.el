;;; cardano-tx-address.el --- Manipulate cardano addresses -*- lexical-binding: t; -*-
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

;;; Commentary:
;;
;; Wrapping around cardano-cli to manage addresses
;;
;;; Code:

(require 'f)
(require 'bech32)
(require 'cl-lib)
(require 'cbor)
(require 'hex-util)
(require 'subr-x)
(require 'cardano-tx-bip32)
(require 'cardano-tx-cli)
(require 'cardano-tx-db)
(require 'cardano-tx-hw)
(require 'cardano-tx-log)
(require 'cardano-tx-utils)

(defun cardano-tx-address--short (address)
  "Shorten ADDRESS for user display."
  (replace-regexp-in-string
   (rx (group "addr" (optional "_test") "1" (= 6 any)) (1+ any)
       (group (= 6 any)) eol)
   "\\1...\\2" address))

(defun cardano-tx-address-new-key (name &optional stake)
  "Create new payment keys under NAME.
If STAKE is non-nil generate stake key."
  (let* ((prefix (expand-file-name name cardano-tx-db-keyring-dir))
         (type (if stake "stake-address" "address"))
         (v-file (concat prefix ".vkey"))
         (s-file (concat prefix ".skey")))
    (if (or (file-exists-p v-file) (file-exists-p v-file))
        (cardano-tx-log 'warn "Skip creating %S key pair, because file-name already exists." name)
      (cardano-tx-cli
       type "key-gen"
       "--verification-key-file" v-file
       "--signing-key-file" s-file)
      (cardano-tx-log 'info "Created new key pair: %S" name))
    (list v-file s-file)))

(defun cardano-tx-address-new-key-files (stake &rest names)
  "Generate the key pairs for each one of NAMES.
STAKE non-nil generates staking keys.  It is the prefix argument.
Files are located in keyring dir together with matching address files."
  (interactive
   (cons current-prefix-arg
         (split-string
          (read-string "How do you want to name your keys(space separate them): "))))
  (cardano-tx-db-load-files
   (mapcan (lambda (name) (cardano-tx-address-new-key name stake)) names)))

(defun cardano-tx-address-stake-pick (&optional allow-none)
  "Select from registered stake keys.
ALLOW-NONE flag for when explicitly skipping a stake key."
  (let ((named-keys (append (when allow-none '(("No Reward")))
                            (--map (cons (car (split-string (cadddr it) "\n")) it) (cardano-tx-db-stake-keys)))))
    (assoc (completing-read "Reward key: " named-keys) named-keys)))

(defun cardano-tx-address-load (spending-type monitor &optional stake-id stake-key-path stake-note)
  "Load and MONITOR addresses from all keys of SPENDING-TYPE.

To include staking set STAKE-NOTE STAKE-ID and STAKE-KEY-PATH."
  (interactive
   (cons (completing-read "Which spending condition? "
                          '("PaymentVerificationKeyShelley_ed25519"
                            "SimpleScriptV2"
                            "PlutusScriptV1")
                          nil t)
         (seq-let (_name id _type path desc) (cardano-tx-address-stake-pick t)
           (list (yes-or-no-p "Watch the created addresses?") id path desc))))
  (-some->> (cardano-tx-db-typed-files-where 'type spending-type)
    (mapcar (-lambda ((id _type payment-key desc))
              (vector nil
                      (if (or (string= "PaymentVerificationKeyShelley_ed25519" spending-type)
                              (string= "PaymentExtendedVerificationKeyShelley_ed25519_bip32" spending-type))
                          (cardano-tx-address-payment payment-key stake-key-path)
                        (cardano-tx-address-from-script payment-key stake-key-path))
                      id stake-id monitor
                      (concat "spend: " (car (split-string desc "\n"))
                              (if stake-note (concat " -- reward: "
                                                     (car (split-string stake-note"\n"))) "")))))
    (emacsql (cardano-tx-db)
             [:insert-or-ignore :into addresses :values $v1])))

(defun cardano-tx-address-payment (vkey-file &optional stake-key)
  "Calculate payment address for VKEY-FILE optionally using STAKE-KEY for rewards."
  (apply #'cardano-tx-cli
         "address" "build"
         "--payment-verification-key-file" vkey-file
         (when stake-key
           (list "--stake-verification-key-file" stake-key))))

(defun cardano-tx-address--hash (cbor-hex)
  "Calculate the hash of a CBOR-HEX verification key encoded byte-string."
  (thread-first
    cbor-hex
    (cbor->elisp)
    (decode-hex-string)
    (substring 0 32) ;; make sure no chaincode sneaks in
    (cardano-tx-blake2-sum)
    (decode-hex-string)))

(defun cardano-tx-address-staking (vkey-file-or-cbor-hex)
  "Construct staking address for key under VKEY-FILE-OR-CBOR-HEX."
  (if (file-exists-p vkey-file-or-cbor-hex)
      (cardano-tx-cli "stake-address" "build"
                      "--stake-verification-key-file" vkey-file-or-cbor-hex)
    (cardano-tx-address-build nil nil
                              (if (string= "--testnet-magic" (car cardano-tx-cli-network-args)) 0 1)
                              'keyhash
                              (cardano-tx-address--hash vkey-file-or-cbor-hex))))

(defun cardano-tx-address-from-script (filename &optional stake-key-file)
  "Calculate the address of a script residing on FILENAME.
Optionally with the STAKE-KEY-FILE."
  (apply #'cardano-tx-cli
         "address" "build"
         "--payment-script-file" (expand-file-name filename)
         (when stake-key-file
           (list "--stake-verification-key-file" stake-key-file))))

(defun cardano-tx-address-stake-registration-cert (stake-vkey &optional leave)
  "Write stake address registration certificate from STAKE-VKEY.
If LEAVE deregister."
  (let* ((action (pcase leave
                   ((or :false (pred null)) "registration")
                   (_ "deregistration")))
         (stake-registration-cert-file (concat stake-vkey "." action ".cert")))
    (cardano-tx-cli "stake-address" (concat action "-certificate")
                    "--stake-verification-key-file" (cardano-tx-drop-chaincode (expand-file-name stake-vkey))
                    "--out-file" stake-registration-cert-file)
    stake-registration-cert-file))

(defun cardano-tx-address-delegation-certificate (pool-id stake-vkey)
  "Create delegation certificate for POOL-ID.
Optionally define the STAKE-VKEY file."
  (let ((delegation-cert-file (make-temp-file "delegation" nil ".cert")))
    (cardano-tx-cli "stake-address" "delegation-certificate"
                    "--stake-verification-key-file" (cardano-tx-drop-chaincode (expand-file-name stake-vkey))
                    "--stake-pool-id" pool-id
                    "--out-file" delegation-cert-file)
    delegation-cert-file))

(defun cardano-tx-address-named ()
  "Wallet list for easier selection."
  (->> (emacsql (cardano-tx-db) [:select [raw note] :from addresses])
       (mapcar (-lambda ((addr note))
                 (cons (concat (cardano-tx-address--short addr) " # "
                               (or (car (split-string note "\n")) ""))
                       addr)))))

(defun cardano-tx-address-pick ()
  "Let the user select an address from currently managed ones."
  (interactive)
  (let ((all-addr (cardano-tx-address-named)))
    (-> (completing-read "Select an address: " all-addr)
        (assoc all-addr)
        cdr cardano-tx-kill-new)))

(defun cardano-tx-address-key-hash (vkey-file)
  "Get the key hash out of the VKEY-FILE."
  (interactive
   (list (read-file-name "Select verification key file: " cardano-tx-db-keyring-dir
                         nil t nil (lambda (n) (string-suffix-p ".vkey" n)))))
  (cardano-tx-kill-new
   (if (member (expand-file-name vkey-file) (mapcar #'caddr (cardano-tx-db-stake-keys)))
       (cardano-tx-cli "stake-address" "key-hash" "--stake-verification-key-file" vkey-file)
     (cardano-tx-cli "address" "key-hash" "--payment-verification-key-file" vkey-file))))

(defun cardano-tx-address-decode (address)
  "Decode ADDRESS string into its representation."
  (-let (((prefix key . bt) (bech32-decode address)))
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
         (encode-hex-string (concat (seq-subseq bt 0 28)))
         (when (and (< (ash key -4) 4) (seq-subseq bt 28))
           (propertize "StakingCredential" 'face 'font-lock-keyword-face))
         (encode-hex-string (concat (seq-subseq bt 28))))
        (string-join " ")
        string-trim)))

(defun cardano-tx-address-build (spend-type spend-hash &optional network-id reward-type reward-hash)
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

;;; HD wallets - wrapping cardano-tx-addresses cli

(defcustom cardano-tx-address-command (executable-find "cardano-address")
  "Which `cardano-tx-address' binary to use."
  :type 'file
  :group 'cardano)

(defun cardano-tx-address-gen-recovery-phrase (size)
  "Create the recovery phrase of SIZE words for HD wallet.
Save it unencrypted on `cardano-tx-db-keyring-dir'."
  (interactive
   (list
    (completing-read "How long shall the recovery phrase be? "
                     (mapcar #'number-to-string (number-sequence 9 24 3))
                     nil t)))
  (let ((phrase-file (expand-file-name "phrase.prv" cardano-tx-db-keyring-dir)))
    (if (file-exists-p phrase-file)
        (cardano-tx-log 'warn "There is already a recovery phrase file on your keyring")
      (with-temp-file phrase-file
        (cardano-tx-cli-reply
         (call-process cardano-tx-address-command nil (current-buffer) nil
                       "recovery-phrase" "generate"
                       "--size" (if (numberp size) (number-to-string size) size))))
      (set-file-modes phrase-file #o600))))

(defun cardano-tx-address-piped (&rest args)
  "Call cardano-address with ARGS transform `current-buffer' from input to output."
  (cardano-tx-log 'debug "%s %s" cardano-tx-address-command
                  (mapconcat #'prin1-to-string args " "))
  (apply #'call-process-region (point-min) (point-max)
         cardano-tx-address-command
         t t nil
         args))

(defun cardano-tx-address--master-key-from-phrase ()
  "Recovery word phrase on buffer to master key."
  (cardano-tx-address-piped "key" "from-recovery-phrase" "Shelley"))

(defun cardano-tx-address--derive-child (path)
  "Piped from buffer derive PATH in str specification from extended key."
  (cardano-tx-address-piped "key" "child" path))

(defun cardano-tx-address-derive-child (root path)
  "Derive public key from ROOT along PATH."
  (with-temp-buffer
    (insert root)
    (cardano-tx-address--derive-child path)
    (buffer-string)))

(defun cardano-tx-address--to-public-key (&optional without-chain-code)
  "Piped from buffer extract the public key optionally WITHOUT-CHAIN-CODE."
  (cardano-tx-address-piped "key" "public"
                            (if without-chain-code
                                "--without-chain-code"
                              "--with-chain-code")))

(defun cardano-tx-address-to-public-key (bech32str &optional without-chain-code)
  "Calculate public keys from BECH32STR optionally WITHOUT-CHAIN-CODE."
  (with-temp-buffer
    (insert bech32str)
    (when (search-backward "_xsk1" nil t)
      (cardano-tx-address--to-public-key without-chain-code))
    (buffer-string)))

(defun cardano-tx-address--new-hd-key (master-key &rest derivations)
  "Create new HD key-pair from MASTER-KEY along BIP32 path string DERIVATIONS."
  (cl-flet ((decode (bech32str)
                    (thread-last bech32str (bech32-decode) (cdr) (apply #'unibyte-string)))
            (encode (bytestring)
                    (thread-first
                      bytestring
                      (encode-hex-string)
                      (cbor<-elisp)
                      (encode-hex-string)))
            (key-type (name path-str is-extended)
                      (format "%s%s%sKeyShelley_ed25519%s"
                              (if (string-suffix-p "2/0" path-str) "Stake" "Payment")
                              (if is-extended "Extended" "")
                              name
                              (if is-extended "_bip32" "")))
            (write (type description cborHex filename)
                   (if (file-exists-p filename)
                       (cardano-tx-log 'warn "Skip creating %S key, because it already exists." filename)
                     (cardano-tx-write-json (cardano-tx-alist type description cborHex) filename)
                     (set-file-modes filename #o600)
                     (cardano-tx-log 'info "Created new key: %S" filename))
                   filename))
    (cl-assert
     (cl-every
      (lambda (path)
        (= (length (split-string path "/"))
           (cond
            ((string-prefix-p "root_" master-key) 5)
            ((string-prefix-p "acct_" master-key) 2))))
      derivations)
     nil "Incorrect path description.")
    (let ((fingerprint (thread-first
                         (decode master-key)
                         (cardano-tx-address-to-public-key)
                         (cardano-tx-hw-fingerprint))))
      (thread-last
        derivations
        (mapcan
         (lambda (path-str)
           (let* ((description (format "[%s]%s" fingerprint path-str))
                  (child (cardano-tx-address-derive-child master-key path-str))
                  (xpub (cardano-tx-address-to-public-key child)))
             (cons
              (write (key-type "Verification" path-str t)
                     description
                     (encode (decode xpub))
                     (cardano-tx-db--new-filename "Verification" description))
              (when (not (string= child xpub))
                (let ((xpriv (decode child)))
                  (list
                   (write (key-type "Signing" path-str t)
                          description
                          (encode (concat (seq-subseq xpriv 0 64)
                                          (seq-subseq (decode xpub) 0 32)
                                          (seq-subseq xpriv 64)))
                          (cardano-tx-db--new-filename "Signing" description)))))))))
        (cardano-tx-db-load-files)))))

(defun cardano-tx-address-new-hd-key-files (&rest paths)
  "Generate the key pairs for HD derivation PATHS."
  (interactive
   (cardano-tx-bip32-expand-derivation-paths
    (read-string "Specify the derivation path (2..4 for ranges): " "1852H/1815H/0H/")))
  (let ((root-key
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "phrase.prv" cardano-tx-db-keyring-dir))
           (cardano-tx-address--master-key-from-phrase)
           (buffer-string))))
    (apply #'cardano-tx-address--new-hd-key
           root-key
           (cl-remove-if-not #'cardano-tx-bip32-validate-hd-path paths))))

;; Derive account addresses

(defun cardano-tx-address-hw-derive-key-files ()
  "Generate public key files for registered Hardware device extended keys."
  (interactive)
  (let* ((xpub (cardano-tx-hw--master-keys))
         (derivations
          (cardano-tx-bip32-expand-derivation-paths
           (read-string (format "Derivation path (2..4 for ranges) %s/"
                                (car (split-string (car xpub))))))))
    (apply #'cardano-tx-address--new-hd-key (elt xpub 4) derivations)))

(defun cardano-tx-address-hw-load (account-pattern network-id)
  "Gather all Hierarchical deterministic addresses.

ACCOUNT-PATTERN is a SQL pattern to match the account.
NETWORK-ID is an int < 32. 0 for testnet, 1 is used for mainnet."
  (interactive
   (seq-let (_name _id fingerprint) (cardano-tx-hw--master-keys)
     (list
      (concat "[" fingerprint "]%")
      (if (string= "--testnet-magic" (car cardano-tx-cli-network-args)) 0 1))))
  (thread-last
    (emacsql (cardano-tx-db)
             [:select [pay:id pay:description pay:cbor-hex stake:id stake:description stake:cbor-hex]
              :from typed-files pay
              :join typed-files stake
              :where (and (like pay:type "Payment%") (like pay:description $s1)
                          (like stake:type "Stake%") (like stake:description $s1))]
             (or account-pattern "[%]%"))
    (mapcar (lambda (row)
              (seq-let (pay:id pay:description pay:cbor-hex stake:id stake:description stake:cbor-hex) row
                (vector nil
                        (cardano-tx-address-build 'keyhash
                                                  (cardano-tx-address--hash pay:cbor-hex) network-id
                                                  'keyhash
                                                  (cardano-tx-address--hash stake:cbor-hex))
                        pay:id stake:id nil
                        (format "spend: %s -- reward: %s" pay:description stake:description)))))
    (emacsql (cardano-tx-db)
             [:insert-or-ignore :into addresses :values $v1])))

(provide 'cardano-tx-address)
;;; cardano-tx-address.el ends here
