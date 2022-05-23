;;; numbers.el --- Separate long integers -*- lexical-binding: t; -*-
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

;;; Commentary:
;;
;;  Overlay to make large numbers readable

;;; Code:

(defgroup numbers nil
  "Make unreadable big integers readable."
  :group 'tools)

(defcustom numbers-separator "_"
  "Character to separate integers."
  :type 'string)

(defcustom numbers-separator-interval 3
  "Number of numbers per group."
  :type 'integer)

(defcustom numbers-separator-ignore-threshold 4
  "Ignore numbers with this many digits.
This prevents separating four digit years."
  :type 'integer)

(defun numbers-set-overlay-properties ()
  "Set properties of numbers overlays.
Consider current setting of user variables."
  ;; In-identifier overlay
  (put 'numbers 'evaporate t)
  (put 'numbers 'before-string numbers-separator))

(numbers-set-overlay-properties)

(defun numbers-overlay-p (overlay)
  "Return whether OVERLAY is an overlay of glasses mode."
  (eq (overlay-get overlay 'category) 'numbers))

(defun numbers-make-overlay (beg end)
  "Create and return readability overlay over the region from BEG to END."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'category 'numbers)
    overlay))

(defun numbers-make-unreadable (beg end)
  "Return identifiers in the region from BEG to END to their unreadable state."
  (dolist (o (overlays-in beg end))
    (when (numbers-overlay-p o)
      (delete-overlay o))))

(defun numbers-make-readable (beg end)
  "Make the identifiers in the region from BEG to END readable."
  (save-excursion
    (save-match-data
      (goto-char beg)
      (while (re-search-forward (rx word-boundary
                                    (group  (1+ digit)
                                            word-boundary)) end t)
        (when (> (length (match-string 1)) numbers-separator-ignore-threshold)
          (dolist (ins (number-sequence
                        (- (match-end 1) numbers-separator-interval)
                        (1+ (match-beginning 1))
                        (- numbers-separator-interval)))
            (numbers-make-overlay ins (1+ ins))))))))

(defun numbers-change (beg end)
  "After-change function updating numbers overlays between BEG to END."
  (numbers-make-unreadable beg end)
  (numbers-make-readable beg end))

(define-minor-mode numbers-separator-mode
  "Separate long numbers."
  :lighter " numsep"
  (if numbers-separator-mode
      (jit-lock-register #'numbers-change)
    (jit-lock-unregister #'numbers-change)))

(provide 'numbers)
;;; numbers.el ends here
