;;; cardano-tests.el --- Testing cardano modules -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera

;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Homepage: https://github.com/Titan-C/cardano.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;;  Regression tests
;;
;;; Code:

(require 'bech32)
(require 'cardano-address)
(require 'cardano-assets)
(require 'cardano-tx)
(require 'cardano-utils)
(require 'ert)

(ert-deftest test-parse-token-bundle ()
  (should (equal (cardano-assets-parse-token-bundle [["61" 5] ["70" 2]])
                 '(("a" . 5) ("p". 2))))
  (should (equal (cardano-assets-parse-token-bundle '(("6672757374726174696f6e" . 2)))
                 '(("frustration" . 2)))))

(ert-deftest address-new-keys ()
  (let ((cardano-address-keyring-dir (make-temp-file "test-addr" t)))
    (cardano-address-new-key-files "hi" "there")
    (should (equal
             (sort (directory-files cardano-address-keyring-dir)
                   #'string-lessp)
             (list "." ".."
                   "hi.skey" "hi.vkey"
                   "there.skey" "there.vkey")))))

(ert-deftest address-validate-hd-path ()
  (pcase-dolist (`(,input ,result)
                 '(("5H" (5H))
                   ("5/2H/8" (5 2H 8))
                   ("PA" nil)
                   ("" nil)
                   (5 nil)
                   ((4 5H 2) (4 5H 2))))
    (should (equal (cardano-address--validate-hd-path input) result))))

(ert-deftest address-constructor ()
  (pcase-dolist (`(,spend-type ,spend-hash ,reward-type ,reward-hash ,network-id ,address ,header)
                 `((keyhash "aa" keyhash "LQ" 0 "addr_test1qpskznz3f3nq0g" 0)
                   (keyhash "aa" nil     nil  0 "addr_test1vpskzt77wgk" #b01100000)
                   (script  "aa" nil     nil  0 "addr_test1wpskzrujzz5" #b01110000)
                   (keyhash "aa" script  "LQ" 1 "addr1y9skznz3urda7x" #b00100001)
                   (nil     nil  script  "LQ" 2 "stake17fx9zpctvfz" #b11110010)
                   (nil     nil  keyhash "LQ" 3 "stake1udx9zlrdnxa" #b11100011)))
    (should (equal (cardano-address-build
                    spend-type spend-hash reward-type reward-hash network-id)
                   address))
    (should (equal (caadr (bech32-decode address)) header))))

(ert-deftest test-address-decode ()
  (should (equal
           (cardano-address-decode "addr_test1wrs527svgl0m0ghkhramqkdae643v0q96d83jku8h8etxrs58smpj")
           "TestNet ScriptHash e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"))
  (should (equal
           (cardano-address-decode "addr1w8433zk2shufk42hn4x7zznjjuqwwyfmxffcjszw5l2ulesdt3jff")
           "MainNet ScriptHash eb188aca85f89b55579d4de10a729700e7113b325389404ea7d5cfe6"))
  (should (equal
           (cardano-address-decode "addr1v9pxtjvpjcjuhlesy7laeqnr4deljmep43vyrl83ffactxs4v2zc7")
           "MainNet PubKeyHash 4265c9819625cbff3027bfdc8263ab73f96f21ac5841fcf14a7b859a")))

(ert-deftest test-get-in ()
  (should (equal (cardano-utils-get-in '((a . 2) (b . 6)) 'b) 6))
  (should (equal (cardano-utils-get-in '((a (b . 6))) 'a 'b) 6))
  (should (equal (cardano-utils-get-in '((a :b 6)) 'a :b) 6))
  (should (equal (cardano-utils-get-in '((a :b 6)) 'a 1) 6))
  (should (equal (cardano-utils-get-in '(a :b 6) 1) :b))
  (should (equal (cardano-utils-get-in '(:a "one" :b 6) :a) "one"))
  (should (equal (cardano-utils-get-in '((ab . "cast")) "ab") "cast"))
  (should (equal (cardano-utils-get-in '(:5 2 (a . 8) (lo ("mi" . "one"))) 'lo "mi") "one"))
  (should (equal (cardano-utils-get-in '(:5 2 (a . 8) (lo ("mi" . "one"))) 'lo "re") nil)))

(ert-deftest test-alist-string-key ()
  (should (equal
           (cardano-utils-alist-key-string '((5 . 2) ("TRE" . 5) (:good (:p . a) (yup . 2))))
           '(("5" . 2) ("TRE" . 5) ("good" ("p" . a) ("yup" . 2))))))

(ert-deftest test-merge-alists ()
  (should (equal
           (cardano-assets-merge-alists #'+
                                        '((lovelace . 1564)
                                          (policy2 (mi . 1)
                                                   (yo . 2)))
                                        '((policy1 (one . 3)
                                                   (two . 7))
                                          (policy2 (yo . 3))
                                          (lovelace . 2111)))
           '((policy1 (one . 3)
                      (two . 7))
             (policy2 (mi . 1)
                      (yo . 5))
             (lovelace . 3675)))))

(ert-deftest test-assets-flatten ()
  (should (equal
           (cardano-assets-flatten '((policy1 (one . 3)
                                              (two . 7))
                                     (policy2 (mi . 1)
                                              (yo . 5))
                                     (lovelace . 3675)))
           '((3 policy1 one) (7 policy1 two) (1 policy2 mi) (5 policy2 yo) 3675))))

(ert-deftest test-key-replace-alists ()
  (should (equal (cardano-utils-alist-key-string
                  '((DEMO . [(("address" . "addr_test1")
                              ("amount"
                               ("DEMO" (:token . 1))
                               (lovelace . 4500000)))
                             (("address" . "addr_test2")
                              (DEMO . t))])
                    ("metadata" (721 ("DEMO"
                                      ("token"
                                       ("DEMO" . "token")
                                       ("description" . "DEMO ia"))))))
                  (cardano-tx--replace-mint-asset-names '(("DEMO" "MOON")
                                                          ("lovelace" "ecalevol"))))

                 '(("MOON" . [(("address" . "addr_test1")
                               ("amount"
                                ("MOON" ("token" . 1))
                                ("ecalevol" . 4500000)))
                              (("address" . "addr_test2")
                               ("MOON" . t))])
                   ("metadata" ("721" ("MOON"
                                       ("token"
                                        ("MOON" . "token")
                                        ("description" . "DEMO ia")))))))))

(ert-deftest test-tx-mints ()
  (should (equal
           (cardano-tx--mints '(("A" "B" "file" (("ONE" . 2) ("TWO" . 5)))
                                ("C" "D" "secd" (("ONE" . 2) ("TWO" . 5)))))
           '("--mint" "5 D.54574f+5 B.54574f+2 D.4f4e45+2 B.4f4e45"
             (("--mint-script-file" "file")
              ("--mint-script-file" "secd"))))))

(provide 'cardano-tests)
;;; cardano-tests.el ends here
