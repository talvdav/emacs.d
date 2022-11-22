(use-package javap-mode)
(use-package kotlin-mode) ;; for gradle kotlin script files

(use-package lsp-java
  :hook (java-mode . lsp))

(add-to-list 'auto-mode-alist '("\\.kts\\'" . kotlin-mode))
