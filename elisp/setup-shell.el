;;;; setup-shell.el
;;;;
;;;; Not currently in use. Sets up shell-specific things. Still have
;;;; to choose between shell and eshell.

;; C-d on an empty line in the shell to terminate the process.
(defun comint-delchar-or-eof-or-kill-buffer (arg)
  (interactive "p")
  (if (null (get-buffer-process (current-buffer)))
      (kill-buffer)
    (comint-delchar-or-maybe-eof arg)))

(add-hook 'shell-mode-hook
          (lambda ()
            (define-key shell-mode-map
              (kbd "C-d") 'comint-delchar-or-eof-or-kill-buffer)))
