(use-package elixir-mode
  :hook (elixir-mode . lsp-deferred)
  :hook (elixir-mode . yas-minor-mode))

(use-package mix)
