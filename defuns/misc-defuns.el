;;;; misc-defuns.el
;;;;
;;;; Also pulled from Magnar.

(defmacro create-simple-keybinding-command (name key)
  `(defmacro ,name (&rest fns)
     (list 'global-set-key (kbd ,key) `(lambda ()
                                         (interactive)
                                         ,@fns))))

(create-simple-keybinding-command f2 "<f2>")
(create-simple-keybinding-command f5 "<f5>")
(create-simple-keybinding-command f6 "<f6>")
(create-simple-keybinding-command f7 "<f7>")
(create-simple-keybinding-command f8 "<f8>")
(create-simple-keybinding-command f9 "<f9>")
(create-simple-keybinding-command f10 "<f10>")
(create-simple-keybinding-command f11 "<f11>")
(create-simple-keybinding-command f12 "<f12>")


(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (call-interactively 'goto-line))
    (linum-mode -1)))

;; Add spaces and proper formatting to linum-mode. It uses more room than
;; necessary, but that's not a problem since it's only in use when going to
;; lines.
(setq linum-format (lambda (line)
  (propertize
   (format (concat " %"
                   (number-to-string
                    (length (number-to-string
                             (line-number-at-pos (point-max)))))
                   "d ")
           line)
   'face 'linum)))

;; TODO: Figure out how this works, looks very cool.
;; Fix kmacro-edit-lossage, it's normal implementation
;; is bound tightly to C-h
; (defun kmacro-edit-lossage ()
;   "Edit most recent 300 keystrokes as a keyboard macro."
;   (interactive)
;   (kmacro-push-ring)
;   (edit-kbd-macro 'view-lossage))

(provide 'misc-defuns)