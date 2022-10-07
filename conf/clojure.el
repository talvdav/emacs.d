(use-package clojure-mode)
(use-package cider)

(when (eq system-type 'gnu/linux)
  (setq exec-path (append exec-path '("~/bin/"))))
