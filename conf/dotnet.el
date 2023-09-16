(add-to-list 'auto-mode-alist '("\\.fsproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.axaml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

(if (< emacs-major-version 29)
(use-package csharp-mode
  :after dotnet))

(use-package fsharp-mode
  :after dotnet)

(use-package dotnet
  :hook (fsharp-mode . dotnet-mode)
  :hook (csharp-mode . dotnet-mode)
  :hook (xml-mode . dotnet-mode)
  :hook (dotnet-mode . lsp-deferred)
  :bind (:map dotnet-mode-map ("<f5>" . dotnet-run)))
