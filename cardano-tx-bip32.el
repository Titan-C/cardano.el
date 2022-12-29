;;; cardano-tx-bip32.el --- BIP32 path utilities -*- lexical-binding: t; -*-
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
;;  BIP32 path utilities
;;
;;; Code:

(require 'subr-x)
(require 'rx)
(require 'cl-lib)

(defun cardano-tx-bip32-list->-path-str (key-path &optional separator)
  "KEY-PATH is list of path symbols on numbers.
Return joined string by SEPARATOR, defaults to `/'."
  (replace-regexp-in-string
   "h" "H"
   (mapconcat #'prin1-to-string key-path (or separator "/"))))

(defun cardano-tx-bip32-validate-hd-path (path)
  "Validate HD derivation PATH and return it normalized.
PATH can be a list of symbols or a string separated by `/', `_' or whitespace."
  (let ((split-path
         (split-string
          (cond
           ((consp path) (cardano-tx-bip32-list->-path-str path))
           ((stringp path) (replace-regexp-in-string (rx (or "h" "'")) "H" path))
           (t ""))
          (rx (or whitespace "/" "_")) t)))
    (when (and split-path (cl-every
                           (lambda (it)
                             (string-match-p (rx bol (1+ digit) (optional "H") eol) it))
                           split-path))
      (string-join split-path "/"))))

(defun cardano-tx-bip32<-path-str (path)
  "Turn into BIP32 integer sequence from string PATH."
  (thread-last
    (split-string (cardano-tx-bip32-validate-hd-path path) "/")
    (mapcar (lambda (entry)
              (if (string-suffix-p "H" entry) ;; Hardened is symbol
                  (thread-first entry
                                (substring  0 -1)
                                (string-to-number)
                                (logxor (ash 1 31)))
                (string-to-number entry))))))

(defun cardano-tx-bip32->path-str (path)
  "Convert from BIP32 integer sequence to readable string PATH."
  (let ((hard-i (ash 1 31)))
    (mapconcat (lambda (idx)
                 (if (< 0(logand idx hard-i))
                     (format "%dH" (logxor idx hard-i))
                   (format "%d" idx)))
               path "/")))

(defun cardano-tx-bip32--parse-index (idx)
  "Parse indexes for derivation.
IDX can be a single integer string or a range a..b.
In both cases can be suffixed with H for hardened.
Returns BIP32 integer list."
  (let ((hardened (if (string-match (rx "H" eol) idx) (ash 1 31) 0))
        (numbers (pcase idx
                   ((rx bol (1+ digit) (? "H") eol)
                    (list (string-to-number idx)))
                   ((and range (rx bol (group (1+ digit)) ".." (group (1+ digit)) (? "H") eol))
                    (number-sequence (string-to-number (match-string 1 range))
                                     (string-to-number (match-string 2 range)))))))
    (mapcar (lambda (x) (+ x hardened)) numbers)))

(defun cardano-tx-bip32--parse-derivation-path (path-spec)
  "Parse PATH-SPEC string definition to list of BIP32 integers."
  (mapcar #'cardano-tx-bip32--parse-index
          (split-string path-spec "/" t)))

(defun cardano-tx-bip32-expand-derivation-paths (path-spec)
  "Expand PATH-SPEC specification into a list of BIP32 integer lists."
  (let ((index-specs (nreverse (cardano-tx-bip32--parse-derivation-path path-spec))))
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
      (mapcar #'cardano-tx-bip32->path-str))))


(provide 'cardano-tx-bip32)
;;; cardano-tx-bip32.el ends here
