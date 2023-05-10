(add-to-list 'auto-mode-alist '("\\.fsproj\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.axaml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml\\'" . xml-mode))
(add-to-list 'auto-mode-alist '("\\.csproj\\'" . xml-mode))

(use-package csharp-mode
  :after dotnet
  :hook (csharp-mode . lsp-deferred)
  :bind (:map csharp-mode-map ("<f5>" . dotnet-run)))

(use-package fsharp-mode
  :after dotnet
  :hook (fsharp-mode . lsp-deferred)
  :bind (:map fsharp-mode-map ("<f5>" . dotnet-run)))

(use-package dotnet
  :hook (fsharp-mode . dotnet-mode)
  :hook (csharp-mode . dotnet-mode)
  :hook (xml-mode . dotnet-mode))
