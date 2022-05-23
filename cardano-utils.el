;;; cardano-utils.el --- Utilities for cardano wallet -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
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
;;
;;; Code:

(require 'dash)
(require 'helm)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'yaml)
(require 'cardano-log)
(require 'cbor)

;; Silence byte-compiler.
(defvar url-http-end-of-headers)

(defun cardano-utils-nw-p (s)
  "Trimmed not white-space string S."
  (when (stringp s)
    (let ((trimed (string-trim s)))
      (and (not (string-empty-p trimed)) trimed))))

(defun cardano-utils-get-in (table &rest keys)
  "From nested hash-map TABLE get element in path of KEYS."
  (--reduce-from (when acc (cardano-utils--reducer-get-in acc it)) table keys))

(defun cardano-utils--reducer-get-in (acc entry)
  "Common function to obtain ENTRY from an ACC object."
  (pcase entry
    ((and (pred integerp) (guard (or (listp acc) (vectorp acc))))
     (if (< entry (length acc)) (elt acc entry) nil))
    ((pred keywordp) (plist-get acc entry))
    ((guard (listp acc)) (alist-get entry acc nil nil #'string=))
    ((guard (hash-table-p acc)) (gethash entry acc))))

(defun cardano-utils-pick (options-name candidates)
  "Simple multiple CANDIDATES picker of type OPTIONS-NAME."
  (helm
   :sources (helm-build-sync-source options-name
              :candidates candidates
              :multiline 400))
  (helm-marked-candidates))

(defun cardano-utils-alist-key-string (alist &optional replacer)
  "Quick & dirty last pass to ensure keys in ALIST are strings.
It really expects a well formed alist.

REPLACER is optionally a function to replace the keys."
  (mapcar (-lambda ((key . rest))
            (cons (->> (pcase key
                         ((pred integerp) (number-to-string key))
                         ((pred keywordp) (substring (symbol-name key) 1))
                         ((pred symbolp) (symbol-name key))
                         (str str))
                       (funcall (or replacer #'identity)))
                  (cond
                   ((consp rest)
                    (cardano-utils-alist-key-string rest replacer))
                   ((vectorp rest)
                    (cl-map 'vector (lambda (it)
                                      (if (consp it)
                                          (cardano-utils-alist-key-string it replacer)
                                        it))
                            rest))
                   (t rest))))
          alist))

(defun cardano-utils--parse-response (status callback)
  "Parse response and log failure STATUS.

CALLBACK processes the response."
  (goto-char url-http-end-of-headers)
  (let ((response (if (cardano-utils-nw-p
                       (buffer-substring (point) (point-max)))
                      (json-read))))
    (if-let ((error-status (plist-get status :error)))
        (let ((err-message (cardano-utils-get-in response "message")))
          (cardano-log 'error "Request status: %S %s" error-status err-message)
          (error err-message))
      (funcall callback response))))

(provide 'cardano-utils)
;;; cardano-utils.el ends here
