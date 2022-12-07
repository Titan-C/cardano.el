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
(require 'cardano-tx-log)
(require 'cardano-tx-address)

(defcustom cardano-tx-hw-command (executable-find "cardano-hw-cli")
  "Which `cardano-tx-hw-cli' binary to use."
  :type 'file
  :group 'cardano)

(defun cardano-tx-hw (&rest args)
  "Call hardware wallet service cli with ARGS."
  (cardano-tx-log 'debug "%s %s" cardano-tx-hw-command (mapconcat #'prin1-to-string args " "))
  (with-temp-buffer
    (cardano-tx-hw-reply
     (apply #'call-process cardano-tx-hw-command nil t nil args))))

(defun cardano-tx-hw-reply (result)
  "Process the RESULT value from an external process and the `current-buffer'."
  (if (= result 0)
      (json-parse-string (buffer-string))
    (let ((err-msg (buffer-string)))
      (cardano-tx-log 'error err-msg)
      (error err-msg))))

(defun cardano-tx-hw--parse-extended-pubkeys (import-pubkey-list)
  "Prepare IMPORT-PUBKEY-LIST from HW device response for database ingestion."
  (seq-map
   (lambda (row)
     (seq-let (path extended-pubkey) row
       (let ((root (bech32-encode "acct_xvk" (decode-hex-string extended-pubkey))))
         (vector
          (cardano-tx-address-fingerprint root)
          (concat "HW" (cardano-tx-hw-bip32->path-str path))
          root))))
   import-pubkey-list))

(defun cardano-tx-hw-request-extended-pubkeys (path-spec)
  "Request extended public keys along expandable PATH-SPEC from hardware device.
Extended public keys are stored directly on the database as master-keys."
  (interactive
   (list (read-string "Derivation path (2..4 for ranges): " "1852H/1815H/0H")))
  (thread-last
    (cardano-tx-hw-expand-derivation-paths path-spec)
    (mapcan (lambda (path) (list "--path" path)))
    (apply #'cardano-tx-hw "pubkey" "query")
    (cardano-tx-hw--parse-extended-pubkeys)
    (emacsql (cardano-tx-db)
             [:insert-or-ignore :into master-keys
              [fingerprint note data]
              :values $v1])))

(defun cardano-tx-hw--master-keys ()
  "Completing read to pick one of the loaded master keys."
  (let* ((accounts (emacsql (cardano-tx-db) [:select [id fingerprint note data] :from master-keys]))
         (select-accounts (mapcar
                           (lambda (row)
                             (seq-let (_ fingerprint note) row
                               (cons (concat (propertize fingerprint 'face 'font-lock-builtin-face) " " note)
                                     row)))
                           accounts))
         (pick (completing-read "Derive from key: " select-accounts)))
    (assoc pick select-accounts)))

(defun cardano-tx-hw-bip32<-path-str (path)
  "Turn into BIP32 integer sequence from string PATH."
  (thread-last
    (split-string (cardano-tx-address--validate-hd-path path) "/")
    (mapcar (lambda (entry)
              (if (string-suffix-p "H" entry) ;; Hardened is symbol
                  (-> entry
                      (substring  0 -1)
                      (string-to-number)
                      (logxor (ash 1 31)))
                (string-to-number entry))))))

(defun cardano-tx-hw-bip32->path-str (path)
  "Convert from BIP32 integer sequence to readable string PATH."
  (let ((hard-i (ash 1 31)))
    (mapconcat (lambda (idx)
                 (if (< 0(logand idx hard-i))
                     (format "%dH" (logxor idx hard-i))
                   (format "%d" idx)))
               path "/")))

(defun cardano-tx-hw--parse-index (idx)
  "Parse indexes for derivation.
IDX can be a single integer string or a range a..b.
In both cases can be sufixed with H for hardened.
Returns BIP32 integer list."
  (let ((numbers (pcase idx
                   ((and n (rx bol (1+ digit) (? "H") eol)) (list (string-to-number n)))
                   ((and range (rx (group (1+ digit)) ".." (group (1+ digit))))
                    (number-sequence (string-to-number (match-string 1 range)) (string-to-number (match-string 2 range)))))))
    (if (string-suffix-p  "H" idx)
        (mapcar (lambda (x) (+ x (ash 1 31))) numbers) numbers)))

(defun cardano-tx-hw--parse-derivation-path (path-spec)
  (mapcar #'cardano-tx-hw--parse-index
          (split-string path-spec "/" t)))

(defun cardano-tx-hw-expand-derivation-paths (path-spec)
  (let ((index-specs (nreverse (cardano-tx-hw--parse-derivation-path path-spec))))
    (thread-last
      (seq-reduce
       (lambda (paths indexes)
         (mapcan
          (lambda (idx)
            (if (null paths)
                (list idx)
              (mapcar (lambda (path)
                        (if (listp path)
                            (cons idx path)
                          (list idx path)))
                      paths)))
          indexes))
       index-specs nil)
      (mapcar #'cardano-tx-hw-bip32->path-str))))

(provide 'cardano-tx-hw)
;;; cardano-tx-hw.el ends here
