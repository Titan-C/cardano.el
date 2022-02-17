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

(ert-deftest cbor-hexstring->ascii ()
  (should (equal (cbor-hexstring->ascii "70617a") "paz"))
  (should (equal (cbor-hexstring->ascii
                  (cbor-string->hexstring "hello world!"))
                 "hello world!")))

(ert-deftest cbor-test-decoding ()
  (pcase-dolist (`(,input . ,expected)
                 `(("1901c8" . 456)
                   ("6448495049" . "HIPI")
                   ("42abcd" . "abcd")
                   ("83050408" . [5 4 8])
                   ("a2636c697306616105" . (("lis" . 6) ("a" . 5)))
                   ("c2820502" . ,(cbor-tag-create :number 2 :content (vector 5 2)))
                   ("a303820208636c697306616105" . ((3 . [2 8]) ("lis" . 6) ("a" . 5)))))
    (should (equal (cbor->elisp input) expected))
    (should (equal (cbor-string->hexstring (cbor<-elisp expected)) input))))

(ert-deftest cbor-test-integer-unpacking ()
  (pcase-dolist (`(,input . ,expected) '(("A" . 65)
                                         ("AA" . 16705)
                                         ("AAAA" . 1094795585)
                                         ("1EQl" . 826626412)))
    (should (equal (cbor--get-ints input) expected)))
  (should (equal (cbor--get-ints "1EQl" t) 1817265457)))

(ert-deftest bech32-test ()
  (dolist (test '("a12uel5l"
                  "A12UEL5L"
                  "an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs"
                  "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
                  "11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j"
                  "split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w"
                  "?1ezyfcl"))
    (-let (((hrp data) (bech32-decode test)))
      (should (equal (downcase test) (bech32-encode hrp data))))))

(provide 'encodings-test)

;;; encodings-test.el ends here
