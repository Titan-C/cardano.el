;;; test-hw.el --- Test HW -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Test HW
;;
;;; Code:

(require 'cardano-tx-hw)

(ert-deftest hw-xpub-parse ()
  (should (equal (cardano-tx-hw--parse-extended-pubkeys
                  [[[2147485500 0 0] "049f"]
                   [[2147485500 2] "8ed6"]
                   [[2147485500 2147485463 2147483650] "d514"]])
                 '(["1c792274" "HW1852H/0/0" "acct_xvk1qj0sk3ckfp"]
                   ["e9634406" "HW1852H/2" "acct_xvk13mtql7pjg0"]
                   ["822d60ee" "HW1852H/1815H/2H" "acct_xvk1652q3j9z7g"]))))

(ert-deftest hw-path-expansion ()
  (should (equal (cardano-tx-hw-expand-derivation-paths
                  "5..6H/7..8H/1..2")
                 '("5H/7H/1" "5H/7H/2" "5H/8H/1" "5H/8H/2"
                   "6H/7H/1" "6H/7H/2" "6H/8H/1" "6H/8H/2"))))

(ert-deftest bip32-path ()
  (should (string= "5H/2/6" (cardano-tx-hw-bip32->path-str (vector (logior (ash 1 31) 5) 2 6))))
  (let ((path  "1852H/6/12H/0"))
    (should
     (string= path (cardano-tx-hw-bip32->path-str (cardano-tx-hw-bip32<-path-str path))))))
