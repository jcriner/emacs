;;;; editing-defuns.el
;;;;
;;;; Functions used for editing purposes.

;; Consistently useful stuff. Should bind this for Elisp mode.
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (insert (eval (read (current-kill 0))))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; Want to put something here to delete the entire line I'm on if
;; kill-region has no region defined.

;; Similarly, want to copy the whole line if "copy region"
;; (kill-ring-save) has no region defined.

;; May want to do this stuff with defadvice.
