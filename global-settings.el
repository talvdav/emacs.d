(defvar *emacsd-directory* user-emacs-directory)
(defvar *conf-directory* "~/.emacs.d/conf")
(setq user-emacs-directory  "~/.emacs.d/emacs/")

  (setq backup-directory-alist '(("." . "~/.emacs-backups"))
        backup-by-copying      t  ; Don't de-link hard links
        version-control        t  ; Use version numbers on backups
        delete-old-versions    nil  ; Automatically delete excess backups:
        kept-new-versions      5  ; how many of the newest versions to keep
        kept-old-versions      5) ; and how many of the old

  (defvar temp-directory "~/.emacs-backups/autosave/")
  (make-directory temp-directory t)

  (setq auto-save-default nil)
  (setq auto-save-directory (concat temp-directory "/autosave")
        auto-save-hash-directory (concat temp-directory "/autosave-hash")
        auto-save-directory-fallback "~/emacs-autosave"
        auto-save-list-file-prefix (concat temp-directory "/autosave-")
        auto-save-hash-p nil
        auto-save-timeout 100
        auto-save-interval 300)
  (make-directory auto-save-directory t)
