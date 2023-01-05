;;; cardano-tx-assets.el --- Manage cardano ledger assets -*- lexical-binding: t; -*-
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
;;
;;; Commentary:
;;
;;  Manage Cardano ledger assets
;;
;;; Code:

(require 'seq)
(require 'dash)
(require 'cbor)
(require 'subr-x)
(require 'cl-lib)

(defun cardano-tx-assets-parse-token-bundle (tokens)
  "TOKENS come as a vector of vectors or as an alist, make them always alist.
Token names are now hex encoded-make them readable."
  (cond ((vectorp tokens)
         (seq-map (-lambda ([tk amount])
                    (cons (decode-hex-string tk) amount))
                  tokens))
        ((consp tokens)
         (seq-map (-lambda ((tk . amount))
                    (cons (decode-hex-string tk) amount))
                  tokens))
        (t (error "Unexpected token Bundle: %S" tokens))))

(defun cardano-tx-assets-format-tokens (value)
  "VALUE is a multi asset make it a string for yaml."
  (cond
   ((numberp value) (format "lovelace: %d" value))
   ;;starting on Mary
   ((stringp (car value))
    (if (numberp (cdr value))
        (format "%S: %d" (if (string= 'lovelace (car value)) 'lovelace (car value)) (cdr value))
      (thread-last (cardano-tx-assets-parse-token-bundle (cdr value))
                   (cardano-tx-assets-format-tokens)
                   (replace-regexp-in-string "^" "  ")
                   (concat (car value) ":\n" ))))
   (t (mapconcat #'cardano-tx-assets-format-tokens value "\n"))))

(defun cardano-tx-assets-group-tokens (flat-token-list)
  "From FLAT-TOKEN-LIST each a string to asset bundle."
  (let ((ht (make-hash-table :test #'equal))
        ret)
    (dolist (entry flat-token-list)
      (-let* (((policy asset amount) entry))
        (push (cons (encode-hex-string asset) amount) (gethash policy ht ()))))
    (maphash (lambda (policy tokens)
               (push (cons policy tokens) ret))
             ht)
    ret))

(defun cardano-tx-assets-hexify (value)
  "Convert asset names in output VALUE to hex strings."
  (mapcar (-lambda ((asset . quantity))
            (if (string= asset 'lovelace)
                (cons asset quantity)
              (cons asset
                    (mapcar (-lambda ((tokenname . amount))
                              (cons (encode-hex-string tokenname) amount))
                            quantity))))
          value))

(defun cardano-tx-assets-flatten (value)
  "Return the flat list representation of VALUE."
  (mapcan (-lambda ((asset . quantity))
            (if (string= asset 'lovelace)
                (list quantity)
              (mapcar (-lambda ((tokenname . amount))
                        (list amount asset tokenname))
                      quantity)))
          value))

(defun cardano-tx-assets-merge-alists (function alist1 alist2)
  "Merge ALIST1 with ALIST2 using FUNCTION.
This assumes FUNCTION will be applied to a pair of numbers."
  (cl-flet ((keys (alist) (mapcar #'car alist)))
    (mapcan (lambda (key)
              (let* ((l-one (alist-get key alist1 nil nil #'string=))
                     (l-two (alist-get key alist2 nil nil #'string=))
                     (result (cond ((and (numberp l-one) (numberp l-two))
                                    (funcall function l-one l-two))
                                   ((and (numberp l-one) (null l-two)) l-one)
                                   ((and (numberp l-two) (null l-one)) (funcall function 0 l-two))
                                   (t (cardano-tx-assets-merge-alists function l-one l-two)))))
                (unless (or (null result) (and (numberp result) (zerop result)))
                  (list (cons key result)))))
            (cl-union (keys alist1) (keys alist2) :test 'equal))))


(provide 'cardano-tx-assets)
;;; cardano-tx-assets.el ends here
