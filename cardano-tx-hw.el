;;; cardano-tx-hw.el --- Hardware wallet interface -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera
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
;;  Hardware wallet interface
;;
;;; Code:

(require 'hex-util)
(require 'seq)
(require 'bech32)
(require 'cardano-tx-bip32)
(require 'cardano-tx-cli)
(require 'cardano-tx-db)
(require 'cardano-tx-log)
(require 'cardano-tx-utils)

(defcustom cardano-tx-hw-command (executable-find "cardano-hw-cli")
  "Which `cardano-tx-hw-cli' binary to use."
  :type 'file
  :group 'cardano)

(defun cardano-tx-hw (&rest args)
  "Call hardware wallet service cli with ARGS."
  (cardano-tx-log 'debug "%s %s" cardano-tx-hw-command (mapconcat #'prin1-to-string args " "))
  (with-temp-buffer
    (let ((response
           (cardano-tx-cli-reply
            (apply #'call-process cardano-tx-hw-command nil t nil args))))
      (or (ignore-errors (json-parse-string response))
          response))))

(defun cardano-tx-hw-fingerprint (xpub-bytestring)
  "Fingerprint XPUB-BYTESTRING, being first 8 chars of wallet-id."
  (substring (cardano-tx-blake2-sum xpub-bytestring 160) 0 8))

(defun cardano-tx-hw--parse-extended-pubkey (pubkey-data)
  "Prepare PUBKEY-DATA from HW device response for database ingestion."
  (seq-let (path extended-pubkey) pubkey-data
    (let* ((raw-key (decode-hex-string extended-pubkey))
           (root (bech32-encode "acct_xvk" raw-key)))
      (vector
       (cardano-tx-hw-fingerprint raw-key)
       (concat "HW" (cardano-tx-bip32->path-str path))
       root))))

(defun cardano-tx-hw-request-extended-pubkeys (path-spec)
  "Request extended public keys along expandable PATH-SPEC from hardware device.
Extended public keys are stored directly on the database as master-keys."
  (interactive
   (list (read-string "Derivation path (2..4 for ranges): " "1852H/1815H/0H")))
  (thread-last
    (cardano-tx-bip32-expand-derivation-paths path-spec)
    (mapcan (lambda (path) (list "--path" path)))
    (apply #'cardano-tx-hw "pubkey" "query")
    (seq-map #'cardano-tx-hw--parse-extended-pubkey)
    (emacsql (cardano-tx-db)
             [:insert-or-ignore :into master-keys
              [fingerprint note data]
              :values $v1]))
  (message "Imported public keys %s" path-spec))

(defun cardano-tx-hw--master-keys ()
  "Completing read to pick one of the loaded master keys."
  (let* ((accounts (emacsql (cardano-tx-db) [:select [id fingerprint note data]
                                             :from master-keys
                                             :where (like data "acct_%")]))
         (select-accounts (mapcar
                           (lambda (row)
                             (seq-let (_ fingerprint note) row
                               (cons (concat "[" (propertize fingerprint 'face 'font-lock-builtin-face) "]" note)
                                     row)))
                           accounts))
         (pick (completing-read "Derive from key: " select-accounts)))
    (assoc pick select-accounts)))

(provide 'cardano-tx-hw)
;;; cardano-tx-hw.el ends here
