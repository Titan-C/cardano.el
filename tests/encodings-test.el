;;; encodings-test.el --- Testing data encodings -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera

;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Homepage: https://github.com/Titan-C/cardano.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Regression tests for encoding and decoding libraries like:
;;
;;  - CBOR
;;  - BECH32
;;
;;; Code:

(require 'ert)
(require 'cbor)
(require 'bech32)
(require 'seq)

(ert-deftest decode-hex-string ()
  (should (equal (decode-hex-string "70617a") "paz"))
  (should (equal (decode-hex-string
                  (encode-hex-string "hello world!"))
                 "hello world!")))

(ert-deftest cbor-test-decoding ()
  (pcase-dolist (`(,input . ,expected)
                 `(("1901c8" . 456)
                   ("f6" . :null)
                   ("a0" . nil)
                   ("6448495049" . "HIPI")
                   ("42abcd" . "abcd")
                   ("83050408" . [5 4 8])
                   ("a2636c697306616105" . (("lis" . 6) ("a" . 5)))
                   ("c2820502" . ,(cbor-tag-create :number 2 :content (vector 5 2)))
                   ("a303820208636c697306616105" . ((3 . [2 8]) ("lis" . 6) ("a" . 5)))))
    (should (equal (cbor->elisp input) expected))
    (should (equal (encode-hex-string (cbor<-elisp expected)) input))))

(ert-deftest cbor-test-integer-unpacking ()
  (pcase-dolist (`(,input . ,expected) '(("A" . 65)
                                         ("AA" . 16705)
                                         ("AAAA" . 1094795585)
                                         ("1EQl" . 826626412)))
    (should (equal (cbor--get-ints input) expected)))
  (should (equal (cbor--get-ints "1EQl" t) 1817265457)))

(ert-deftest bech32-test-bech32 ()
  (dolist (test '("a12uel5l"
                  "A12UEL5L"
                  "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
                  "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
                  "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
                  "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
                  "?1ezyfcl"))
    (seq-let (encoding hrp &rest data) (bech32--decode test)
      (should (eq encoding 'bech32))
      (should (equal (downcase test) (bech32--encode 'bech32 hrp data))))))

(ert-deftest bech32-encode-multiinput ()
  (dolist (empty-seq '(nil "" []))
    (should (string= (bech32-encode "a" empty-seq) "a12uel5l")))
  (dolist (data (list "hello" (string-to-list "hello") (vconcat (string-to-list "hello"))))
    (should (string= (bech32-encode "test?" data) "test?1dpjkcmr0f9cjl4"))))

(ert-deftest bech32-fails ()
  (dolist (test '(" 1nwldj5"
                  (format "%c1axkwrx" #x7f)
                  (format "%c1eym55h" #x80)
                  "an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx",
                  "pzry9x0s0muk"
                  "1pzry9x0s0muk"
                  "x1b4n0q5v"
                  "li1dgmt3"
                  (format "de1lg7wt%c" #xff)
                  "A1G7SGD8"
                  "10a06t8"
                  "1qzzfhee"))
    (should-error (bech32--decode test))))

(ert-deftest bech32-test-bech32m ()
  (dolist (test '("A1LQFN3A"
                  "a1lqfn3a"
                  "an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6"
                  "abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx"
                  "11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8"
                  "split1checkupstagehandshakeupstreamerranterredcaperredlc445v"
                  "?1v759aa"))
    (seq-let (encoding hrp &rest data) (bech32--decode test)
      (should (eq encoding 'bech32m))
      (should (equal (downcase test) (bech32--encode 'bech32m hrp data))))))

(ert-deftest bech32-fails-m ()
  (dolist (test '(" 1xj0phk"
                  (format "%c1g6xzxy" #x7f)
                  (format "%c1vctc34" #x80)
                  "an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4",
                  "qyrz8wqd2c9m"
                  "1qyrz8wqd2c9m"
                  "y1b0jsk6g"
                  "lt1igcx5c0"
                  "in1muywd"
                  "mm1crxm3i"
                  "au1s5cgom"
                  "M1VUXWEZ"
                  "16plkw9"
                  "1p2gdwpf"))
    (should-error (bech32--decode test))))

;;; encodings-test.el ends here
