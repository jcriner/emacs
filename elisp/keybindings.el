;;;; keybindings.el
;;;; 
;;;; Where all major keybindings get set.

;;;-------------------------------------------------
;;; Make Emacs act more like Unix shell keybindings.

;; First with backward word killing.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region) ; remap kill-region

;; Some notes on the above: Firstly, what was C-x C-k, again? Hmm.
;; Secondly, I could just have C-w work as was supposed to originally
;; if it wasn't set on a region. On the other hand, leaving C-x C-k as
;; is: when no region is selected, I would love if it would kill the
;; whole line instead of just after the point.
;; Similarly, copy should work on current line if not set to region.

;; Remap C-h to delete-backward-char.
;; (Note: M-h used to be bound to 'mark-paragraph.)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
;; Help is already bound to F1, anyway.
(global-set-key (kbd "C-c h") 'help)

;;;---------------------------------------------------

;; Beginning of line, but do what I really mean.
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

;; Auto-indent on RET globally by default. (Maybe sane-defaults.el
;; should have this)
;;
;; This is a pain for things like python mode. Guess I'll need mode
;; specific things against this...
(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)
;; Python gets a more limited (and less annoying) version. 
(eval-after-load 'python 
  '(define-key python-mode-map (kbd "RET") 'newline-and-indent))

;; Expansion binding
(global-set-key (kbd "C-.") 'dabbrev-expand) ; will make hippie, later

;; Improve 'open-line' functionality.
(global-set-key (kbd "C-o") 'smart-open-line)
(global-set-key (kbd "C-S-o") 'smart-open-line-above)

;; Make the shell more convenient.
(global-set-key (kbd "C-z") 'shell)


;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Zap to char (consider M-Z for backwards killing to char).
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)


;;;--------------------------------------------------
;;; Some Vim-based stuff.

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'iy-go-to-char)
(global-set-key (kbd "M-M") 'iy-go-to-char-backward)

;; ace-jump-mode
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)


;;;--------------------------------------------------


;; Window commands:
(global-set-key (kbd "C-x 3") 'split-window-right-and-move-there-dammit)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)

;; Window switching
(windmove-default-keybindings) ; Shift + direction

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)


;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)


;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Eval and replace sexp
(global-set-key (kbd "C-c e") 'eval-and-replace)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

;; Jump from file to containing directory
(autoload 'dired-jump "dired")
(autoload 'dired-jump-other-window "dired")
(global-set-key (kbd "C-x C-j") 'dired-jump)
(global-set-key (kbd "C-x 4 C-j") 'dired-jump-other-window)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Display and edit occurances of regexp in buffer
(global-set-key (kbd "C-c o") 'occur)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

;; Expand region (by semantic units)
; TODO: Is this the right keybinding for this?
(global-set-key (kbd "M-@") 'er/expand-region)

;; Switch C-l to a prefix command.
(global-unset-key "\C-l")
(defvar ctl-l-map (make-keymap)
     "Keymap for local bindings and functions, prefixed by (^L)")
(define-key global-map "\C-l" 'Control-L-prefix)
(fset 'Control-L-prefix ctl-l-map)

;; Standard remapping.

(global-set-key (kbd "C-l C-l") 'recenter)
;; TODO: These will be altered to be the commands I actually want.
(define-key ctl-l-map "r"  'replace-string)
(define-key ctl-l-map "R"  'replace-regexp)
(define-key ctl-l-map "q"  'query-replace)
(define-key ctl-l-map "Q"  'query-replace-regexp)
(define-key ctl-l-map "h"  'command-history)

(provide 'keybindings)
