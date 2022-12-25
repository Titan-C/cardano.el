;;; cardano-tx-utils.el --- Utilities for cardano wallet -*- lexical-binding: t; -*-
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
;;
;;; Code:

(require 'hex-util)
(require 'dash)
(require 'f)
(require 'helm)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'yaml)
(require 'cardano-tx-log)
(require 'cbor)

;; Silence byte-compiler.
(defvar url-http-end-of-headers)

(defgroup cardano-tx nil
  "Cardano transaction editor."
  :group 'tools)

(defun cardano-tx-nw-p (s)
  "Trimmed not white-space string S."
  (when (stringp s)
    (let ((trimed (string-trim s)))
      (and (not (string-empty-p trimed)) trimed))))

(defun cardano-tx-get-in (table &rest keys)
  "From nested hash-map TABLE get element in path of KEYS."
  (--reduce-from (when acc (cardano-tx--reducer-get-in acc it)) table keys))

(defun cardano-tx--reducer-get-in (acc entry)
  "Common function to obtain ENTRY from an ACC object."
  (pcase entry
    ((and (pred integerp) (guard (or (listp acc) (vectorp acc))))
     (if (< entry (length acc)) (elt acc entry) nil))
    ((pred keywordp) (plist-get acc entry))
    ((guard (listp acc)) (alist-get entry acc nil nil #'string=))
    ((guard (hash-table-p acc)) (gethash entry acc))))

(defun cardano-tx-pick (options-name candidates)
  "Simple multiple CANDIDATES picker of type OPTIONS-NAME."
  (helm
   :sources (helm-build-sync-source options-name
              :candidates candidates
              :multiline t))
  (helm-marked-candidates))

(defun cardano-tx-alist-key-string (alist &optional replacer)
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
                    (cardano-tx-alist-key-string rest replacer))
                   ((vectorp rest)
                    (cl-map 'vector (lambda (it)
                                      (if (consp it)
                                          (cardano-tx-alist-key-string it replacer)
                                        it))
                            rest))
                   (t rest))))
          alist))

(defun cardano-tx--parse-response (status callback)
  "Parse response and log failure STATUS.

CALLBACK processes the response."
  (goto-char url-http-end-of-headers)
  (let ((response (if (cardano-tx-nw-p
                       (buffer-substring (point) (point-max)))
                      (json-read))))
    (if-let ((error-status (plist-get status :error)))
        (let ((err-message (cardano-tx-get-in response "message")))
          (cardano-tx-log 'error "Request status: %S %s" error-status err-message)
          (error err-message))
      (funcall callback response))))

(defun cardano-tx-blake2-sum (string-or-buffer &optional size)
  "Calculate blake2 checksum for STRING-OR-BUFFER.
Output SIZE in bits, default 224. Return as hexstring."
  (let ((size (or size 224)))
    (if (= (mod size 8) 0)
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (cond
           ((stringp string-or-buffer) (insert string-or-buffer))
           ((bufferp string-or-buffer) (insert-buffer-substring string-or-buffer))
           (t (error "Wrong type")))
          (call-process-region (point-min) (point-max) "b2sum" t t nil "-l" (number-to-string size) "-b")
          (buffer-substring-no-properties 1 (1+ (/ size 4))))
      (user-error "Blake2 requires an output size multiple of 8."))))

(defun cardano-tx-escape-non-alphanum (str)
  "Replace non alphanum or dot with underscore on str."
  (replace-regexp-in-string
   (rx (+ (not (in alnum "."))))
   "_" str))

(defun cardano-tx-clean-filename (filename &optional dir)
  "Remove any non alphanumeric or square braces from FILENAME."
  (let ((file-name
         (expand-file-name
          (cardano-tx-escape-non-alphanum
           (file-name-nondirectory filename))
          (file-name-directory filename))))
    (if (file-exists-p file-name)
        (if (yes-or-no-p (format "File %s exists. Overwrite?" filename))
            file-name
          (cardano-tx-clean-filename (read-file-name "Pick a name for the file" dir)))
      file-name)))

(defun cardano-tx-drop-chaincode (file)
  "Create new key FILE without chaincode in case it has, otherwise pass."
  (let ((data (json-parse-string (f-read file) :object-type 'alist)))
    (if (not (string-suffix-p "_bip32"
                              (cardano-tx-get-in data 'type)))
        file
      (let ((temp-file (make-temp-file "cardano-temp")))
        (f-write
         (json-serialize
          (list (cons 'type
                      (replace-regexp-in-string (rx (or "Extended" "_bip32")) "" (cardano-tx-get-in data "type")))
                (cons 'cborHex
                      (thread-first
                        (cardano-tx-get-in data 'cborHex)
                        (cbor->elisp)
                        (decode-hex-string)
                        (substring 0 32)
                        (encode-hex-string)
                        (cbor<-elisp)
                        (encode-hex-string)))))
         'utf-8 temp-file)
        temp-file))))

(provide 'cardano-tx-utils)
;;; cardano-tx-utils.el ends here
