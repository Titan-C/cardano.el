;;; cardano-tx-cli.el --- Wrapper around the cardano cli -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; This file is not part of GNU Emacs.
;;
;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Wrapping around cardano-cli
;;
;;; Code:

(require 'cardano-tx-log)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'yaml)
(require 'yaml-mode)

(defcustom cardano-tx-cli-command (executable-find "cardano-cli")
  "Which `cardano-tx-cli' binary to use."
  :type 'file
  :group 'cardano-tx)

(defcustom cardano-tx-cli-node-socket "/run/cardano/cardano.socket"
  "Where is the node socket."
  :type 'file
  :group 'cardano-tx)

(defcustom cardano-tx-cli-network-args '("--testnet-magic" "1097911063")
  "Declares the network magic needed."
  :type '(repeat string)
  :group 'cardano-tx)

(defvar cardano-tx-cli-skip-network-args
  (list "key" "key-gen" "key-hash" "txid" "view" "build-raw" "hash-script-data"
        "version" "policyid" "assemble"
        "registration-certificate"
        "deregistration-certificate"
        "delegation-certificate")
  "Commands that don't require adding the network arguments.")

(defun cardano-tx-cli (&rest args)
  "Call the cli interface connected to the node socket pass ARGS."
  (let ((process-environment (list (concat "CARDANO_NODE_SOCKET_PATH=" cardano-tx-cli-node-socket)))
        (cmd (if (seq-intersection cardano-tx-cli-skip-network-args args)
                 args
               (append args cardano-tx-cli-network-args))))
    (cardano-tx-log 'debug "%s %s" cardano-tx-cli-command (mapconcat #'prin1-to-string cmd " "))
    (with-temp-buffer
      (cardano-tx-cli-reply
       (apply #'call-process cardano-tx-cli-command nil (current-buffer) nil
              cmd)))))

(defun cardano-tx-cli-reply (result)
  "Process the RESULT value from an external process and the `current-buffer'."
  (if (= result 0)
      (string-trim (buffer-string))
    (goto-char (point-min))
    (when (re-search-forward "Usage:" nil 'end)
      (backward-sentence))
    (let ((err-msg (string-trim (buffer-substring-no-properties (point-min) (point)))))
      (cardano-tx-log 'error err-msg)
      (error err-msg))))

(defun cardano-tx-cli-pretty-yaml-message (obj)
  "Encode OBJ into yaml and display on mini-buffer."
  (with-temp-buffer
    (yaml-mode)
    (thread-first obj
                  yaml-encode
                  insert)
    (font-lock-ensure)
    (message (buffer-string))))

(defun cardano-tx-cli-tip ()
  "Display in mini-buffer current chain tip."
  (interactive)
  (thread-first (cardano-tx-cli "query" "tip")
                json-read-from-string
                cardano-tx-cli-pretty-yaml-message))

(defun cardano-tx-cli-version ()
  "Print the current cli version."
  (interactive)
  (message (cardano-tx-cli "version")))

(provide 'cardano-tx-cli)
;;; cardano-tx-cli.el ends here
