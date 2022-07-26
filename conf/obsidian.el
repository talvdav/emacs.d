(use-package obsidian
  :straight (:type git :host github :repo "licht1stein/obsidian.el")
  :config (obsidian-specify-path "~/Documents/Dokumentation.Wiki")
  :custom
  (obsidian-inbox-directory "Inbox"))
