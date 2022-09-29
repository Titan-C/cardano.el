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
   (version "2022-08-16")
   (source (origin
            (method url-fetch)
            (uri "https://github.com/input-output-hk/cardano-wallet/releases/download/v2022-08-16/cardano-wallet-v2022-08-16-linux64.tar.gz")
            (sha256
             (base32
              "1vwd7n098grv495817dkhd9mq46ax9hl1mgb7vyimhf5rafjp5zl"))))
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
