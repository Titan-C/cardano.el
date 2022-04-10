(use-modules (guix)
             (gnu packages linux)
             (gnu packages version-control)
             (gnu packages emacs-xyz)
             (guix build-system emacs)
             (guix build-system copy)
             (guix git-download)
             (guix licenses))

(define %source-dir (dirname (current-filename)))

(define cardano-wallet
  (package
   (name "cardano-wallet")
   (version "2022-01-18")
   (source (origin
            (method url-fetch)
            (uri "https://hydra.iohk.io/build/11906483/download/1/cardano-wallet-v2022-01-18-linux64.tar.gz")
            (sha256
             (base32
              "1gwgchh0n5h6qpqkaww5hx27wdddd6ips2r5vmqqc4r7myam68gs"))))
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
   (inputs
    (list cardano-wallet))
   (propagated-inputs
    (list emacs-dash emacs-yaml-mode emacs-yaml
          emacs-yasnippet emacs-helm emacs-f))
   (home-page "https://github.com/Titan-C/cardano.el")
   (synopsis "An emacs interface to Cardano")
   (description "Wrapping Cardano cli tools")
   (license gpl3+)))

cardano-el
