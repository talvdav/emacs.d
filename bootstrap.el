(setq gc-cons-threshold 64000000)

(add-hook 'after-init-hook (lambda ()
                             ;; restore after startup
                             (setq gc-cons-threshold 800000)))


;;(setq custom-file "~/.emacs.d/custom.el") 
;;
;;(if (file-exists-p custom-file)
;;    (load-file "~/.emacs.d/custom.el"))

;; Initialize package sources
(require 'package)

;(setq package-archives '(("melpa" . "https://melpa.org/packages/")
;                         ("org" . "https://orgmode.org/elpa/")
;                         ("elpa" . "https://elpa.gnu.org/packages/")))
;
;(package-initialize)
;(unless package-archive-contents
;  (package-refresh-contents))
;
;;; Initialize use-package on non-Linux platforms
;(unless (package-installed-p 'use-package)
;  (package-install 'use-package))
;
;(require 'use-package)
;(setq use-package-always-ensure t)

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

(use-package all-the-icons)
(all-the-icons-install-fonts t)

(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name *config-directory*))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

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

(setq *config-directory* user-emacs-directory)
(setq user-emacs-directory  "~/.emacs.d/cache/")

(use-package no-littering)

(setq aut-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(org-babel-tangle-file "~/.emacs.d/Init.org")

(if (file-exists-p "~/.emacs")
	   (delete-file "~/.emacs"))

(load-file "~/.emacs.d/init.el")

