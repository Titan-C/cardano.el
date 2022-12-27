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

(require 'ert)
(require 'seq)
(require 'cardano-tx-hw)

(ert-deftest hw-xpub-parse ()
  (should (equal (seq-map
                  #'cardano-tx-hw--parse-extended-pubkey
                  [[[2147485500 0 0] "049f"]
                   [[2147485500 2] "8ed6"]
                   [[2147485500 2147485463 2147483650] "d514"]])
                 '(["11c74e74" "HW1852H/0/0" "acct_xvk1qj0sk3ckfp"]
                   ["6dca348b" "HW1852H/2" "acct_xvk13mtql7pjg0"]
                   ["7f3550e9" "HW1852H/1815H/2H" "acct_xvk1652q3j9z7g"]))))
