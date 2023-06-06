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

(ert-deftest cbor-spec-cycle ()
  (let ((specs `("00" 0
                 "01" 1
                 "0a" 10
                 "17" 23
                 "1818" 24
                 "1819" 25
                 "1864" 100
                 "18ff" 255
                 "190100" 256
                 "1903e8" 1000
                 "19ffff" 65535
                 "1a00010000" 65536
                 "1a000f4240" 1000000
                 "1affffffff" #xFFFFFFFF
                 "1b0000000100000000"  #x100000000
                 "1b000000e8d4a51000" 1000000000000
                 "1bffffffffffffffff"  18446744073709551615
                 "20"  -1
                 "29"  -10
                 "37"  -24
                 "3818"  -25
                 "3863"  -100
                 "38ff"  -256
                 "390100"  -257
                 "3903e7"  -1000
                 "39ffff"  -65536
                 "3a00010000"  -65537
                 "3affffffff"  -4294967296
                 "3b0000000100000000"  -4294967297
                 "3bffffffffffffffff" -18446744073709551616
                 ;; byte array
                 "4401020304" "01020304"
                 ;; text
                 "60" ""
                 "6161" "a"
                 "617a" "z"
                 ;; "626161" "aa" Conflicts with hex encoded byte-string
                 "6449455446"  "IETF"
                 "62225c"  "\"\\"
                 "62c3bc"  "ü"
                 "63e6b0b4"  "水"
                 ;; Arrays
                 "80" []
                 "83010203" [1 2 3]
                 "811864" [100]
                 "8120" [-1]
                 "8301820203820405" [1 [2 3] [4 5]]
                 "98190102030405060708090a0b0c0d0e0f101112131415161718181819" [1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]
                 ;; maps
                 "a201020304" ((1 . 2) (3 . 4))
                 "a26161016162820203" (("a" . 1) ("b" . [2 3]))
                 "a56161614161626142616361436164614461656145"  (("a" . "A") ("b" . "B") ("c" . "C") ("d" . "D") ("e" . "E"))
                 ;; Tagged
                 "c074323031332d30332d32315432303a30343a30305a" ,(cbor-tag-create :number 0 :content "2013-03-21T20:04:00Z")
                 "c11a514b67b0"  ,(cbor-tag-create :number 1 :content 1363896240)
                 "f4" :false
                 "f5" t)))
    (cl-loop for (input expected) on specs by #'cddr do
             (should (equal (cbor->elisp input) expected))
             (should (equal (encode-hex-string (cbor<-elisp expected)) input)))))

(ert-deftest cbor-spec-one-way ()
  (let ((specs '("40" "" ;; empty byte array
                 ;; Indefinite length
                 "9f01820203820405ff" [1 [2 3] [4 5]]
                 "9fff" []
                 "bf6346756ef563416d7421ff" (("Fun" . t) ("Amt" . -2)))))
    (cl-loop for (input expected) on specs by #'cddr do
             (should (equal (cbor->elisp input) expected)))))

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
