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

(require 'cardano-tx-log)
(require 'cardano-tx-address)

(defcustom cardano-tx-hw-command (executable-find "cardano-hw-cli")
  "Which `cardano-tx-hw-cli' binary to use."
  :type 'file
  :group 'cardano)

(defun cardano-tx-hw (&rest args)
  (cardano-tx-log 'debug "%s %s" cardano-tx-hw-command (mapconcat #'prin1-to-string args " "))
  (with-temp-buffer
    (apply #'call-process cardano-tx-hw-command nil t nil args)
    (goto-char (point-min))
    (buffer-string)))

(setq hw-keys
      (cardano-tx-hw "pubkey" "query"
                     "--path" "1852H/1815H/0H"
                     "--path" "1852H/1815H/1H"
                     "--path" "1852H/1815H/2H"))


(with-current-buffer (get-buffer-create "*test*")
  (erase-buffer)
  (insert hw-keys)
  (goto-char (point-min))
  (while (search-forward "'" nil t)
    (replace-match "\""))
  (goto-char (point-min))
  (seq-map
   (-lambda ([path extkey])
     (let* ((main-key (->> (cbor-hexstring->ascii extkey)
                           (string-to-list)
                           (bech32-encode "acct_xvk")))
            (stake
             (with-temp-buffer
               (insert main-key)
               (cardano-tx-address-derive-child "2/0")
               (buffer-string))))
       (with-temp-buffer
         (insert main-key)
         (cardano-tx-address-derive-child "0/0")
         (cardano-tx-address-piped "address" "payment" "--network-tag" "preview")
         (cardano-tx-address-piped "address" "delegation" stake)
         (buffer-string))))
   (json-parse-buffer))
  )

(with-temp-buffer
  (insert-file-contents
   (expand-file-name "phrase.prv" cardano-tx-db-keyring-dir))
  (cardano-tx-address-master-key-from-phrase)
  (cardano-tx-address-derive-child "1852H/1815H/0H")
  (cardano-tx-address-public-key )
  (->
   (buffer-string)
   ;; bech32-decode
   ;; cdr length
   )
  )

(defun cardano-tx-hw-bip32-path (path)
  "Turn into integer sequence from string PATH."
  (mapcar (lambda (entry)
            (let* ((entry (symbol-name entry))
                   (hardened? (string-suffix-p "H" entry)))
              (->
               (if hardened? (substring entry 0 -1) entry)
               (string-to-number)
               (logxor (ash (if hardened? 1 0) 31)))))
          (cardano-tx-address--validate-hd-path path)))

(cardano-tx-hw-bip32-path "1852H/1815H/0H")


(provide 'cardano-tx-hw)
;;; cardano-tx-hw.el ends here
