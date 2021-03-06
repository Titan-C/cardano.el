;;; cardano-tx-log.el --- Logging System -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021-2022 Óscar Nájera

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
;;  Logging System inspired by the elfeed log
;;
;;  https://github.com/skeeto/elfeed/blob/master/elfeed-log.el
;;
;;; Code:

(require 'subr-x)

(defvar cardano-tx-log-buffer-name "*cardano-log*"
  "Name of buffer used for logging events.")

(defvar cardano-tx-log-level 'info
  "Lowest type of messages to be logged.")

(defun cardano-tx-log-buffer ()
  "Return the buffer for `cardano-tx-log', creating it as needed."
  (if-let ((buffer (get-buffer cardano-tx-log-buffer-name)))
      buffer
    (with-current-buffer (generate-new-buffer cardano-tx-log-buffer-name)
      (special-mode)
      (current-buffer))))

(defun cardano-tx-log--level-number (level)
  "Return a relative level number for LEVEL."
  (pcase level
    ('debug -10)
    ('info 0)
    ('warn 10)
    ('error 20)
    (_ -10)))

(defun cardano-tx-log--level-face (level)
  "Return the font-lock-face for the give LEVEL."
  (pcase level
    ('debug 'font-lock-comment-face)
    ('info 'font-lock-doc-face)
    ('warn 'font-lock-warning-face)
    ('error 'font-lock-keyword-face)))

(defun cardano-tx-log (level fmt &rest objects)
  "Write log message FMT at LEVEL to log buffer.

LEVEL should be a symbol: debug, info, warn, error.
FMT must be a string suitable for `format' given OBJECTS as arguments."
  (when (>= (cardano-tx-log--level-number level)
            (cardano-tx-log--level-number cardano-tx-log-level))
    (let ((inhibit-read-only t))
      (with-current-buffer (cardano-tx-log-buffer)
        (goto-char (point-max))
        (insert
         (format "[%s] [%s]: %s\n"
                 (thread-first (format-time-string "%Y-%m-%d %H:%M:%S")
                               (propertize  'face 'font-lock-constant-face))
                 (thread-first level (symbol-name) (upcase)
                               (propertize 'face (cardano-tx-log--level-face level)))
                 (apply #'format fmt objects)))))))

(provide 'cardano-tx-log)
;;; cardano-tx-log.el ends here
