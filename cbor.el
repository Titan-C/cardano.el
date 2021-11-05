;;; cbor.el --- CBOR utilities -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "25.1") (dash "2.19.0"))
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
;; Utility to decode cbor
;;
;; Heavily inspired by the work in
;;
;; https://inqlab.net/git/guile-cbor.git/
;;
;;; Code:

(require 'seq)
(require 'dash)

(defun cbor-hexstring->ascii (hex-string)
  "Convert HEX-STRING into ASCII string."
  (->> (string-to-list hex-string)
       (-partition 2)
       (--map (string-to-number (concat it) 16))
       concat))

(defun cbor-string->hexstring (s)
  "Encode the string S into a hex string."
  (mapconcat
   (lambda (it) (format "%02x" it))
   (string-to-list s)
   ""))

(defvar cbor--raw nil
  "BYTESTRING port for access.")

(defun cbor--get-ints (string &optional little)
  "Convert byte STRING to integer.
Default to big-endian unless LITTLE is non-nil."
  (let ((work-str (if little string (reverse string))))
    (apply #'logior
           (-zip-with #'ash (string-to-list work-str)
                      (number-sequence 0 (* 8 (length work-str)) 8)))))

(defun cbor--consume! (bytes)
  "Consume N BYTES from the source string."
  (prog1 (substring cbor--raw 0 bytes)
    (setq cbor--raw (substring cbor--raw bytes))))

(defun cbor--get-argument! (info)
  "Get argument meaning given INFO."
  (cond
   ((< info 24) info)
   ((= info 24) (cbor--get-ints (cbor--consume! 1)))
   ((= info 25) (cbor--get-ints (cbor--consume! 2)))
   ((= info 26) (cbor--get-ints (cbor--consume! 4)))
   ((= info 27) (cbor--get-ints (cbor--consume! 8)))
   ((= info 31) 'indefinite-length)
   (t 'malformed-input)))

(defun cbor--get-data-array! (item-count)
  "Get ITEM-COUNT number of array elements."
  (-> (let (result)
        (if (eq 'indefinite-length item-count)
            (let ((value (cbor--get-data-item!)))
              (while (not (eq value 'break))
                (push value result)
                (setq value (cbor--get-data-item!)))
              result)
          (dotimes (_ item-count result)
            (push (cbor--get-data-item!) result))))
      reverse vconcat))

(defun cbor--get-data-map! (pair-count)
  "Get PAIR-COUNT number of map elements."
  (let (result)
    (if (eq 'indefinite-length pair-count)
        (let ((key (cbor--get-data-item!)))
          (while (not (eq key 'break))
            (push (cons key (cbor--get-data-item!)) result)
            (setq key (cbor--get-data-item!)))
          result)
      (dotimes (_ pair-count result)
        (push (cons (cbor--get-data-item!) (cbor--get-data-item!)) result)))))

(defun cbor--get-data-item! ()
  "Read a single CBOR data item."
  (let* ((initial-byte (string-to-char (cbor--consume! 1)))
         ;; major type is in the higher-order 3 bits
         (major-type (ash initial-byte -5))
         ;; additional information is in the lower-order 5 bits (string-to-number "00011111" 2) =>31
         (additional-information (logand initial-byte 31))
         (argument (cbor--get-argument! additional-information)))
    (pcase major-type
      ;; unsigned integer
      (0 argument)
      ;; negative integer
      (1 (- -1 argument))
      ;; bytestring are hex encoded
      (2 (cbor-string->hexstring (cbor--consume! argument)))
      ;; Text strings
      (3 (cbor--consume! argument))
      ;; array of data
      (4 (cbor--get-data-array! argument))
      ;; map of pairs of data items
      (5 (cbor--get-data-map! argument))
      ;; simple values
      (7
       (pcase additional-information
         (20 nil)
         (21 t)
         (22 nil)
         (31 'break))))))

(defun cbor-hex-p (str)
  "Test if STR is a hex only."
  (string-match-p (rx line-start (+ hex) line-end) str))

(defun cbor->elisp (byte-or-hex-string)
  "Convert BYTE-OR-HEX-STRING into an Emacs Lisp object."
  (let ((cbor--raw (if (cbor-hex-p byte-or-hex-string)
                       (cbor-hexstring->ascii byte-or-hex-string)
                     byte-or-hex-string)))
    (cbor--get-data-item!)))

(provide 'cbor)
;;; cbor.el ends here
