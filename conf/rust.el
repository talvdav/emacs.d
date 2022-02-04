(use-package rust-mode
  :hook (rust-mode . lsp-deferred))

(use-package toml-mode)
(add-to-list 'auto-mode-alist '("\\.toml\\'" . toml-mode))
