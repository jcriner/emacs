;;;; setup-lisp.el
;;;;
;;;; Configure lisp-specific modes.
;;;; Includes config for Elisp, Scheme, and Paredit setup.

;;; paredit config

;; Extra function for special paren behavior on RET.
(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")
  (defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
          (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

;; Make some changes to Lisp modes using Paredit.
(defun lisp-modes-hook ()
  (paredit-mode t) ; turn on Paredit for Lisp files
  ;; Special RET behavior for opening and closing parens.
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (show-paren-mode t)
 
  ;; ElDoc specifics. (Find similar for Scheme, CL.)
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (eldoc-add-command 'electrify-return-if-match))

;; Then add the lisp mode changes to the actual language modes.
;; Should write a simple macro to expand into these.
(add-hook 'emacs-lisp-mode-hook 'lisp-modes-hook)
(add-hook 'scheme-mode-hook 'lisp-modes-hook)
(add-hook 'lisp-mode-hook 'lisp-modes-hook) ; Common Lisp

;; Also important for scheme: (put with scheme stuff)
;; This sets my 'run-scheme' program to chez.
(setq scheme-program-name "chez-scheme")

(provide 'setup-lisp)
