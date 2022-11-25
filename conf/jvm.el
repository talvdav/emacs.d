(use-package clojure-mode)
(use-package cider)

(use-package javap-mode)
(use-package kotlin-mode) ;; for gradle kotlin script files
(use-package scala-mode)

(use-package lsp-java
  :hook (java-mode . lsp)
  :hook (scala-mode . lsp)
  :hook (kotlin-mode . lsp))

(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))

(when (eq system-type 'gnu/linux)
  (setq exec-path (append exec-path '("~/bin/"))))
