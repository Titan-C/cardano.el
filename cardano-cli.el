;;; cardano-cli.el --- Wrapper around the cardano cli -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Óscar Nájera
;;
;; Author: Óscar Nájera <https://oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Version: 0.0.1
;; Homepage: https://github.com/Titan-C/cardano.el
;; Package-Requires: ((emacs "27.1"))
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

(require 'seq)
(require 'libyaml)

(defgroup cardano-cli nil
  "Integration with cardano cli"
  :group 'cardano)

(defcustom cardano-cli-command (executable-find "cardano-cli")
  "Which cardano-cli binary to use."
  :type 'file)

(defcustom cardano-cli-node-socket "/tmp/cardano.socket"
  "Where is the node socket."
  :type 'file)

(defcustom cardano-cli-network-args '("--testnet-magic" "1097911063")
  "Declares the network magic needed."
  :type '(repeat string))

(defvar cardano-cli-skip-network-args
  (list "key-gen" "key-hash" "txid" "view" "build-raw" "hash-script-data"
        "version" "policyid")
  "Commands that don't requiry adding the network args.")


(defun cardano-cli (&rest args)
  "Call the cli interface connected to the node socket pass ARGS."
  (let ((process-environment (list (concat "CARDANO_NODE_SOCKET_PATH=" cardano-cli-node-socket)))
        (cmd (if (seq-intersection cardano-cli-skip-network-args args)
                 args
               (append args cardano-cli-network-args))))
    (message "Calling cardano-cli with args %S" cmd)
    (with-temp-buffer
      (let ((result
             (apply #'call-process cardano-cli-command nil (list (current-buffer) "/tmp/carerr") nil
                    cmd)))
        (if (= result 1)
            (with-current-buffer (find-file-noselect "/tmp/carerr")
              (goto-char (point-min))
              (when (re-search-forward "Usage:" nil 'end)
                (backward-sentence))
              (error (string-trim (buffer-substring-no-properties (point-min) (point)))))
          (string-trim (buffer-string)))))))

(defun cardano-cli-json->yaml (json-string)
  "Convert JSON-STRING to yaml."
  (yaml-dump (json-parse-string json-string)))

(defun cardano-cli-tip ()
  "Display in minibuffer current chain tip."
  (interactive)
  (message (cardano-cli-json->yaml (cardano-cli "query" "tip"))))

(defun cardano-cli-version ()
  "Print the current cli version."
  (interactive)
  (cardano-cli "version"))

(provide 'cardano-cli)
;;; cardano-cli.el ends here
