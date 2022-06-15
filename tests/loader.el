;;; runner.el --- test runner -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Óscar Nájera
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
