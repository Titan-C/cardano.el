;;; cardano-assets.el --- Manage cardano ledger assets -*- lexical-binding: t; -*-
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

(defun cardano-assets-parse-token-bundle (tokens)
  "TOKENS come as a vector of vectors or as an alist, make them always alist.
Token names are now hex encoded-make them readable."
  (cond ((vectorp tokens)
         (seq-map (-lambda ([tk amount])
                    (cons (cbor-hexstring->ascii tk) amount))
                  tokens))
        ((consp tokens)
         (seq-map (-lambda ((tk . amount))
                    (cons (cbor-hexstring->ascii tk) amount))
                  tokens))
        (t (error "Unexpected token Bundle: %S" tokens))))

(defun cardano-assets-format-tokens (value)
  "VALUE is a multi asset make it a string for yaml."
  (cond
   ((numberp value) (format "lovelace: %d" value))
   ;;starting on Mary
   ((stringp (car value)) (format "%s: %s" (car value)
                                  (let ((tokens (cdr value)))
                                    (if (numberp tokens)
                                        tokens
                                      (thread-last (cardano-assets-parse-token-bundle tokens)
                                        cardano-assets-format-tokens
                                        (replace-regexp-in-string "^" "  ")
                                        (concat "\n"))))))
   (t (mapconcat #'cardano-assets-format-tokens value "\n"))))

(defun cardano-assets-flatten (value)
  "Return the flat list representation of VALUE."
  (apply #'append
         (mapcar (-lambda ((asset . quantity))
                   (if (string= asset 'lovelace)
                       (list quantity)
                     (mapcar (-lambda ((tokenname . amount))
                               (list amount asset tokenname))
                             quantity)))
                 value)))

(defun cardano-assets-merge-alists (function alist1 alist2)
  "Merge ALIST1 with ALIST2 using FUNCTION."
  (cl-flet ((keys (alist) (mapcar #'car alist)))
    (cl-loop with keys = (cl-union (keys alist1) (keys alist2) :test 'equal)
             for k in keys collect
             (let ((l-one (alist-get k alist1 nil nil #'string=))
                   (l-two (alist-get k alist2 nil nil #'string=)))
               (cons k (cond ((and (numberp l-one) (numberp l-two))
                              (funcall function l-one l-two))
                             ((and (numberp l-one) (null l-two)) l-one)
                             ((and (numberp l-two) (null l-one)) l-two)
                             (t (cardano-assets-merge-alists function l-one l-two))))))))

(provide 'cardano-assets)
;;; cardano-assets.el ends here
