;;;; editing-defuns.el
;;;;
;;;; Functions used for editing purposes.

;; Consistently useful stuff. Should bind this for Elisp mode.
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

;; open-line stuff grabbed from bbatsov
(defun smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(defun smart-open-line ()
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

;; Want to put something here to delete the entire line I'm on if
;; kill-region has no region defined.

;; Similarly, want to copy the whole line if "copy region"
;; (kill-ring-save) has no region defined.

;; May want to do this stuff with defadvice.
