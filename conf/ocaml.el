
 (add-to-list 'load-path "C:\Users\thomas\AppData\Local\opam\playground\share/emacs/site-lisp")
     (require 'ocp-indent)

 (let ((opam-share (ignore-errors (car (process-lines "opam" "var" "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

(use-package tuareg-mode)