;;; runner.el --- test runner -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera
;;
;; Author: Óscar Nájera <hi@oscarnajera.com>
;; Maintainer: Óscar Nájera <hi@oscarnajera.com>
;; Created: April 08, 2022
;; Modified: April 08, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/titan/runner
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  test runner
;;
;;; Code:


(let ((default-directory "~/dev/doom-emacs/.local/straight/build-28.1/"))
  (if (file-exists-p default-directory)
      (normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path default-directory)
