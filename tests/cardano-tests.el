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

(require 'dash)
(require 'bech32)
(require 'f)
(require 'emacsql)
(require 'emacsql-sqlite3)
(require 'cardano-tx-address)
(require 'cardano-tx-assets)
(require 'cardano-tx)
(require 'cardano-tx-utils)
(require 'cardano-wallet)
(require 'ert)

(ert-deftest test-parse-token-bundle ()
  (should (equal (cardano-tx-assets-parse-token-bundle [["61" 5] ["70" 2]])
                 '(("a" . 5) ("p". 2))))
  (should (equal (cardano-tx-assets-parse-token-bundle '(("6672757374726174696f6e" . 2)))
                 '(("frustration" . 2)))))

(defmacro with-keyring (&rest body)
  "Fixture to execute BODY with address keyring temporary dir."
  `(let ((cardano-tx-db-keyring-dir (make-temp-file "test-addr" t)))
     (when (and cardano-tx-db-connection (emacsql-live-p cardano-tx-db-connection))
       (emacsql-close cardano-tx-db-connection))
     (unwind-protect
         (progn ,@body)
       (emacsql-close cardano-tx-db-connection)
       (sleep-for 0 1)  ;; wait for close
       (setq cardano-tx-db-connection nil)
       (delete-directory cardano-tx-db-keyring-dir t))))

(ert-deftest address-new-keys ()
  (with-keyring
   (cardano-tx-address-new-key-files nil "hi" "there")
   (cardano-tx-address-new-key-files t "stake")
   (execute-kbd-macro (vconcat (kbd "RET") )
                      1
                      (lambda ()
                        (cardano-tx-address-load "PaymentVerificationKeyShelley_ed25519" t)))
   (should (equal
            (sort (directory-files cardano-tx-db-keyring-dir)
                  #'string-lessp)
            (list "." ".."
                  "cardano.db"
                  "hi.skey" "hi.vkey"
                  "stake.skey" "stake.vkey"
                  "there.skey" "there.vkey")))
   (should (equal 2 (length (cardano-tx-db-address--list))))))

(ert-deftest address-new-hd-keys ()
  (with-keyring
   (cardano-tx-address-gen-recovery-phrase 12)
   (cardano-tx-address-new-hd-key-files "1852H/1815H/0H/0/0")
   (should (equal
            (sort (directory-files cardano-tx-db-keyring-dir)
                  #'string-lessp)
            (list "." ".."
                  "1852H_1815H_0H_0_0.skey"
                  "1852H_1815H_0H_0_0.vkey"
                  "cardano.db"
                  "phrase.prv"
                  )))

   (execute-kbd-macro (vconcat (kbd "RET") )
                      1
                      (lambda ()
                        (cardano-tx-address-load "PaymentVerificationKeyShelley_ed25519" t)))
   (should (equal 1 (length (cardano-tx-db-address--list))))))

(ert-deftest address-validate-hd-path ()
  (pcase-dolist (`(,input ,result)
                 '(("5H" (5H))
                   ("5/2H/8" (5 2H 8))
                   ("5 2h_8" (5 2H 8))
                   ("5/2'_8" (5 2H 8))
                   ("PA" nil)
                   ("" nil)
                   (5 nil)
                   ((4 5h 2) (4 5H 2))
                   ((4 5H 2) (4 5H 2))))
    (should (equal (cardano-tx-address--validate-hd-path input) result))))

(ert-deftest address-new-script ()
  (with-keyring
   (cardano-tx-address-new-key-files nil "hi")
   ;; new single sig script
   (execute-kbd-macro (vconcat "sig" (kbd "RET") "/hi.vkey" (kbd "RET") (kbd "C-c C-c"))
                      1
                      (lambda () (cardano-tx-new-script)))
   (execute-kbd-macro (vconcat (kbd "RET")) ;; script with stake
                      1
                      (lambda () (cardano-tx-address-load "SimpleScriptV2" t)))
   (should (equal 1 (length (cardano-tx-db-address--list))))))

(ert-deftest address-constructor ()
  (pcase-dolist (`(,spend-type ,spend-hash ,reward-type ,reward-hash ,network-id ,address ,header)
                 `((keyhash "aa" keyhash "LQ" 0 "addr_test1qpskznz3f3nq0g" 0)
                   (keyhash "aa" nil     nil  0 "addr_test1vpskzt77wgk" #b01100000)
                   (script  "aa" nil     nil  0 "addr_test1wpskzrujzz5" #b01110000)
                   (keyhash "aa" script  "LQ" 1 "addr1y9skznz3urda7x" #b00100001)
                   (nil     nil  script  "LQ" 2 "stake17fx9zpctvfz" #b11110010)
                   (nil     nil  keyhash "LQ" 3 "stake1udx9zlrdnxa" #b11100011)))
    (should (equal (cardano-tx-address-build
                    spend-type spend-hash reward-type reward-hash network-id)
                   address))
    (should (equal (cadr (bech32-decode address)) header))))

(ert-deftest test-address-decode ()
  (should (equal
           (cardano-tx-address-decode "addr_test1wrs527svgl0m0ghkhramqkdae643v0q96d83jku8h8etxrs58smpj")
           "TestNet ScriptHash e1457a0c47dfb7a2f6b8fbb059bdceab163c05d34f195b87b9f2b30e"))
  (should (equal
           (cardano-tx-address-decode "addr1w8433zk2shufk42hn4x7zznjjuqwwyfmxffcjszw5l2ulesdt3jff")
           "MainNet ScriptHash eb188aca85f89b55579d4de10a729700e7113b325389404ea7d5cfe6"))
  (should (equal
           (cardano-tx-address-decode "addr1v9pxtjvpjcjuhlesy7laeqnr4deljmep43vyrl83ffactxs4v2zc7")
           "MainNet PubKeyHash 4265c9819625cbff3027bfdc8263ab73f96f21ac5841fcf14a7b859a")))

(ert-deftest test-get-in ()
  (should (equal (cardano-tx-get-in '((a . 2) (b . 6)) 'b) 6))
  (should (equal (cardano-tx-get-in '((a (b . 6))) 'a 'b) 6))
  (should (equal (cardano-tx-get-in '((a :b 6)) 'a :b) 6))
  (should (equal (cardano-tx-get-in '((a :b 6)) 'a 1) 6))
  (should (equal (cardano-tx-get-in '(a :b 6) 1) :b))
  (should (equal (cardano-tx-get-in '(:a "one" :b 6) :a) "one"))
  (should (equal (cardano-tx-get-in '((ab . "cast")) "ab") "cast"))
  (should (equal (cardano-tx-get-in '(:5 2 (a . 8) (lo ("mi" . "one"))) 'lo "mi") "one"))
  (should (equal (cardano-tx-get-in '(:5 2 (a . 8) (lo ("mi" . "one"))) 'lo "re") nil)))

(ert-deftest test-alist-string-key ()
  (should (equal
           (cardano-tx-alist-key-string '((5 . 2) ("TRE" . 5)
                                          (:good (:p . a) (yup . 2))
                                          (:datum . [1 yup ((with . 2) (no . 3))])))
           '(("5" . 2) ("TRE" . 5) ("good" ("p" . a) ("yup" . 2))
             ("datum" . [1 yup (("with" . 2) ("no" . 3))])))))

(ert-deftest test-datum-find ()
  (should (equal
           (mapcar #'cardano-tx-datum-hash
                   '((("datum" . "Satoshi"))
                     (("datumhash" . "ba"))
                     (("datumhash" . ""))))
           '("062f86b7cc31be2c92bd647b4613227ae3e8902ff3c47cd440a4c36676e483ac" "ba" nil))))

(ert-deftest test-merge-alists ()
  (should (equal
           (cardano-tx-assets-merge-alists #'+
                                           '((lovelace . 1564)
                                             (policy2 (mi . 1)
                                                      (yo . 2)))
                                           '((policy1 (one . 3) (two . 7))
                                             (policy2 (yo . 3))
                                             (lovelace . 2111)))
           '((policy1 (one . 3) (two . 7))
             (policy2 (mi . 1) (yo . 5))
             (lovelace . 3675))))

  (should (equal (cardano-tx-assets-merge-alists #'-
                                                 '((lovelace . 2111))
                                                 '((policy1 (one . 3) (two . 7))
                                                   (policy2 (yo . 3))
                                                   (lovelace . 2111)))
                 '((policy1 (one . -3) (two . -7))
                   (policy2 (yo . -3))))))

(ert-deftest test-assets-flatten ()
  (should (equal
           (cardano-tx-assets-flatten '((policy1 (one . 3) (two . 7))
                                        (policy2 (mi . 1) (yo . 5))
                                        (lovelace . 3675)))
           '((3 policy1 one) (7 policy1 two) (1 policy2 mi) (5 policy2 yo) 3675))))

(ert-deftest test-key-replace-alists ()
  (should (equal (cardano-tx-alist-key-string
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

(ert-deftest test-wallet-spend ()
  (should (equal (cardano-wallet-payment
                  '(("address" . "addr_test1qz")
                    ("amount" ("lovelace" . 156548316)
                     ("hocu" ("" . 16) ("sauo" . 5))
                     ("mito" ("" . 56) ("pul" . 7)))))
                 '(:address  "addr_test1qz"
                   :amount (:quantity 156548316
                            :unit  "lovelace")
                   :assets  [(:policy_id "hocu"
                              :asset_name ""
                              :quantity 16)
                             (:policy_id "hocu"
                              :asset_name "7361756f"
                              :quantity 5)
                             (:policy_id "mito"
                              :asset_name ""
                              :quantity 56)
                             (:policy_id "mito"
                              :asset_name "70756c"
                              :quantity 7)]))))

(ert-deftest test-tx-balance ()
  (with-keyring
   (emacsql (cardano-tx-db) [:insert-or-ignore :into addresses [raw] :values $v1]
            '(["addr_test1qz"] ["addr_test1qp"]))
   (cardano-tx-db-utxo-load '(("8bdfcfa7faa87#0"
                               ("address" .
                                "addr_test1qp")
                               ("value" ("lovelace" . 72605594)))
                              ("fdacb43b67119#0"
                               ("address" .
                                "addr_test1qp")
                               ("value" ("lovelace" . 50847374)))))
   ;; (cardano-tx-db-utxo--list)

   (should (equal (cardano-tx-available-balance(cardano-tx--parse-yaml "
inputs:
  - utxo: 8bdfcfa7faa87#0
  - utxo: fdacb43b67119#0
mint:
  abc64:
    assets:
      test: 64
"))
                  "      lovelace: 123452968
      abc64:
        \"test\": 64"))))

(ert-deftest test-tx-witness-query ()
  (should (equal (cardano-tx-witness-query nil) []))
  (should (equal (cardano-tx-witness-query '("first"))
                 [:union :select [path] :from typed-files :where (like path "%first%.vkey")]))
  (should (equal (cardano-tx-witness-query '("second" "first"))
                 [:union :select [path] :from typed-files
                  :where (or (like path "%second%.vkey") (like path "%first%.vkey"))])))

(ert-deftest test-witnesses ()
  (with-keyring
   (cardano-tx-db-insert-address "addr1qp" nil 1)
   (emacsql (cardano-tx-db) [:insert-or-ignore :into typed-files [type path] :values $v1]
            '["PaymentVerificationKeyShelley_ed25519" "signer.vkey"])
   (cardano-tx-db-utxo-load '(("fdacb43b67119#0"
                               ("address" . "addr1qp")
                               ("value" ("lovelace" . 50847374)))))
   (should (equal '("signer.skey")
                  (cardano-tx-witnesses
                   (cardano-tx--parse-yaml "collateral: fdacb43b67119#0"))))))

;; test utility
(defun print-to-file (data filename)
  "Store DATA object into FILENAME."
  (with-temp-file filename
    (prin1 data (current-buffer))))

(defun read-from-file (filename)
  "Retrieves from FILENAME the object."
  (with-temp-buffer
    (insert-file-contents filename)
    (cl-assert (eq (point) (point-min)))
    (read (current-buffer))))

(defconst test-dir (f-dirname (f-this-file)))

;; (cardano-tx-db-utxo-load '(("8bdfcfa7faa87#0"
;;                            ("address" .
;;                             "addr_test1qp")
;;                            ("value" ("lovelace" . 72605594)))
;;                           ("fdacb43b67119#0"
;;                            ("address" .
;;                             "addr_test1qp")
;;                            ("value" ("lovelace" . 50847374)))))

(ert-deftest test-tx-instructions ()
  (with-keyring
   (cardano-tx-address-new-key-files nil "first")
   (cardano-tx-address-new-key-files t "stake")
   (cardano-tx-address-new-key "test-stake" t)
   (--zip-with (should (string-match-p it other))
               (read-from-file (expand-file-name "all-features.inst" test-dir))
               (cl-letf (((symbol-function 'cardano-tx-cli)
                          (lambda (&rest _args)
                            ;; policy id of test. Skips all calls and queries
                            "243a9175537bcf690eed5bc4227709793cf2c575e0bd045b7c2ba83a")))
                 (let ((default-directory cardano-tx-db-keyring-dir))
                   (-> (with-current-buffer
                           (find-file-noselect (expand-file-name "all-features.yml" test-dir))
                         (cardano-tx--input-buffer))
                       (cardano-tx--build-instructions)
                       (flatten-tree)
                       ;; (print-to-file "all-features.inst")
                       ))))))

(provide 'cardano-tests)
;;; cardano-tests.el ends here
