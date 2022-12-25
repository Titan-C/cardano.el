;;; bip32-tests.el --- BIP32 tests -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  BIP32 tests
;;
;;; Code:

(require 'ert)
(require 'cardano-tx-bip32)

(ert-deftest validate-hd-path ()
  (pcase-dolist (`(,input ,result)
                 '(("5H" "5H")
                   ("5/2H/8" "5/2H/8")
                   ("5 2h_8" "5/2H/8")
                   ("5/2'_8" "5/2H/8")
                   ("PA" nil)
                   ("" nil)
                   (5 nil)
                   ((4 5h 2) "4/5H/2")   ;; maybe bad type
                   ((4 5H 2) "4/5H/2"))) ;; maybe bad type
    (should (equal (cardano-tx-bip32-validate-hd-path input) result))))


(ert-deftest bip32-path ()
  (should (string= "5H/2/6" (cardano-tx-bip32->path-str (vector (logior (ash 1 31) 5) 2 6))))
  (let ((path  "1852H/6/12H/0"))
    (should
     (string= path (cardano-tx-bip32->path-str (cardano-tx-bip32<-path-str path))))))

(ert-deftest hw-path-expansion ()
  (should (equal (cardano-tx-bip32-expand-derivation-paths
                  "5..6H/7..8H/1..2")
                 '("5H/7H/1" "5H/7H/2" "5H/8H/1" "5H/8H/2"
                   "6H/7H/1" "6H/7H/2" "6H/8H/1" "6H/8H/2"))))
