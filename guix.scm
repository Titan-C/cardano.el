(use-modules (guix)
             (gnu packages linux)
             (gnu packages version-control)
             (gnu packages emacs)
             (gnu packages emacs-xyz)
             (guix build-system emacs)
             (guix build-system copy)
             (guix git-download)
             (guix licenses))

(define %source-dir (dirname (current-filename)))
(display %source-dir)

(define-public cardano-wallet
  (package
   (name "cardano-wallet")
   (version "2022-04-27")
   (source (origin
            (method url-fetch)
            (uri "https://hydra.iohk.io/build/14067440/download/1/cardano-wallet-v2022-04-27-linux64.tar.gz")
            (sha256
             (base32
              "0isdk01iwr0zcm7nzrhv94rwf4dy4m36m2rgyywg05vc2q69yggh"))))
   (build-system copy-build-system)
   (arguments
    '(#:install-plan
      '(("." "bin/" #:include-regexp ("cardano-" "bech32")))))
   (synopsis "cardano node, cli, address, bech32 and wallet")
   (description
    "The cardano node and wallet from iohk")
   (home-page "https://github.com/input-output-hk/cardano-wallet")
   (license asl2.0)))

(define cardano-el
  (package
   (name "cardano-el")
   (version "git")
   (source (local-file %source-dir
                       #:recursive? #t
                       #:select? (git-predicate %source-dir)))
   (build-system emacs-build-system)
   (native-inputs
    (list util-linux git emacs))
   (inputs
    (list cardano-wallet))
   (propagated-inputs
    (list emacs-dash emacs-yaml-mode emacs-yaml
          emacs-yasnippet emacs-helm emacs-f
          emacs-emacsql emacs-emacsql-sqlite3))
   (home-page "https://github.com/Titan-C/cardano.el")
   (synopsis "An emacs interface to Cardano")
   (description "Wrapping Cardano cli tools")
   (license gpl3+)))

cardano-el
