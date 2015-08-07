;;;; setup-ido.el
;;;;
;;;; Ido-mode config.

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Add some hooks.
(add-hook
 'ido-setup-hook
 (lambda ()
   ;; Go straight home
   (define-key ido-file-completion-map
     (kbd "~")
     (lambda ()
       (interactive)
       (cond
        ((looking-back "~/") (insert "projects/"))
        ((looking-back "/") (insert "~/"))
        (:else (call-interactively 'self-insert-command)))))

   ;; Use C-w to go back up a dir to better match normal usage of C-w
   ;; - insert current file name with C-x C-w instead.
   (define-key ido-file-completion-map 
     (kbd "C-w") 'ido-delete-backward-updir)
   (define-key ido-file-completion-map 
     (kbd "C-x C-w") 'ido-copy-current-file-name)))

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode t)

(provide 'setup-ido)
;;; setup-ido.el ends here
