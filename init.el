;;;; .emacs.d/init.el, Jacob Criner

;;; Will almost certainly want to break this up into muliple files, soon.
;;; When doing so, consider using 'org-mode' structure.


;; Structure for multiple files:
;; -- global settings, look and feel
;; -- per language/s settings (Lisps, Ruby, Forth)


;;;---------------------------------------------
;;; Look and feel settings.

;; Disable splash screen at start-up.
(setq inhibit-splash-screen t)

;; Make M-x easier to press on standard keyboard.
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Make Emacs act more like Unix shell keybindings.
; First with backward word killing.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region) ; remap kill-region

; Remap C-h to delete-backward-char.
; -- mapped to what DEL actually calls as a function.
; Note: M-h used to be bound to 'mark-paragraph.
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)

; Remapping help functionality.
(global-set-key (kbd "C-?") 'help-command)

; Display column as well as line number.
(column-number-mode)

; Highlight matching parens.
(show-paren-mode t)



;;;---------------------------------------------
;;; Package management.

;; Load up the Marmalade repos.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

;; What does this do, exactly?
(when (not package-archive-contents)
  (package-refresh-contents))

;; List of packages I expect to have anywhere.
(defvar my-packages '(smex
		      ido-ubiquitous
		      haskell-mode
		      auctex
		      paredit
		      paredit-menu
		      )
  "A list of packages to ensure are installed at launch.")


;; Install packages that are not present on current machine.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))



;;;---------------------------------------------
;;; Configure packages.

;; ido-mode config
; (Note: not sure what ido-ubiq does vs. smex. Find out)
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; AUCTeX config
(setq TeX-auto-save t)
(setq TeX-parse-self t) ; Provides AUCTex support for packages 'included'
                        ; in a TeX document.
; (setq-default TeX-master nil) ; Make AUCTeX aware of multi-file document
				 ; structure.



;; paredit config (may need to start moving things to separate files)

; Extra function for special paren behavior on RET.
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

; Make some changes to Lisp modes using Paredit.
(defun lisp-modes-hook ()
  (paredit-mode t) ; turn on Paredit for Lisp files
  ; Special RET behavior for opening and closing parens.
  (local-set-key (kbd "RET") 'electrify-return-if-match)
  (show-paren-mode t)
 
  ; ElDoc specifics. (Find similar for Scheme, CL.)
  (turn-on-eldoc-mode)
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round)
  (eldoc-add-command 'electrify-return-if-match))

; Then add the lisp mode changes to the actual language modes.
; Should write a simple macro to expand into these.
(add-hook 'emacs-lisp-mode-hook 'lisp-modes-hook)
(add-hook 'scheme-mode-hook 'lisp-modes-hook)
(add-hook 'lisp-mode-hook 'lisp-modes-hook) ; Common Lisp

; Also important for scheme: (put with scheme stuff)
; This sets my 'run-scheme' program to chez.
(setq scheme-program-name "chez-scheme")

;;---------------------------------------------
;; Report time to start up Emacs.
; put something here to do that.


