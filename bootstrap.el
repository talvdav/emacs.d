(defvar *config-directory* user-emacs-directory)
(setq user-emacs-directory  "~/.emacs.d/cache/")

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(use-package org)

(use-package evil
  :config
  (evil-mode 1))

(blink-cursor-mode 0)

(use-package evil-escape
  :straight t
  :diminish
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode 1))

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name *config-directory*))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)))

  (push '("conf-unix" . conf-unix) org-src-lang-modes))

(with-eval-after-load 'org
  ;; This is needed as of Org 9.2
  (require 'org-tempo)

  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp")))

(use-package no-littering)
(use-package all-the-icons
  :init (all-the-icons-install-fonts t))


(setq aut-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(org-babel-tangle-file "~/.emacs.d/Init.org")

(if (file-exists-p "~/.emacs")
    (delete-file "~/.emacs"))

(load-file "~/.emacs.d/init.el")
