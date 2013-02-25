;;;; keybindings.el
;;;; 
;;;; Where all major keybindings get set.

;;;-------------------------------------------------
;;; Make Emacs act more like Unix shell keybindings.

;; First with backward word killing.
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region) ; remap kill-region

;; Remap C-h to delete-backward-char.
;; (Note: M-h used to be bound to 'mark-paragraph.)
(global-set-key (kbd "C-h") 'backward-delete-char-untabify)
(global-set-key (kbd "M-h") 'backward-kill-word)
;; Help is already bound to F1, anyway.

;;;---------------------------------------------------

(global-set-key (kbd "C-.") 'dabbrev-expand) ; will make hippie, later

;; Smart M-x
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Per Steve Yegge's advice:
(global-set-key (kbd "C-x C-m") 'smex)


;; Make the shell more convenient.
(global-set-key (kbd "C-z") 'shell)


;; Transpose stuff with M-t
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(global-set-key (kbd "M-t l") 'transpose-lines)
(global-set-key (kbd "M-t w") 'transpose-words)
(global-set-key (kbd "M-t s") 'transpose-sexps)
(global-set-key (kbd "M-t p") 'transpose-params)

;; Zap to char
(global-set-key (kbd "M-z") 'zap-up-to-char) ;TODO: find defn
(global-set-key (kbd "M-Z") 'zap-to-char)


;;;--------------------------------------------------
;;; Some Vim-based stuff.

;; iy-go-to-char - like f in Vim
(global-set-key (kbd "M-m") 'jump-char-forward)
(global-set-key (kbd "M-M") 'jump-char-backward)

;; vim's ci and co commands
(global-set-key (kbd "M-I") 'change-inner)
(global-set-key (kbd "M-O") 'change-outer)

;;;--------------------------------------------------


;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; Jump to a definition in the current file. (This is awesome)
(global-set-key (kbd "C-x C-i") 'ido-imenu)


;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
;(global-set-key (kbd "C-c y") 'bury-buffer)
;(global-set-key (kbd "C-c r") 'revert-buffer)
;(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)
;(global-set-key (kbd "C-x C-b") 'ibuffer)
;; TODO: Figure out what the above do.

;; Window switching
(windmove-default-keybindings) ; Shift + direction

;; Navigation bindings
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "<home>") 'beginning-of-buffer)
(global-set-key (kbd "<end>") 'end-of-buffer)


;; Webjump let's you quickly search google, wikipedia, emacs wiki
(global-set-key (kbd "C-x g") 'webjump)
(global-set-key (kbd "C-x M-g") 'browse-url-at-point)


;; Completion at point (M-tab is harder to press.)
(global-set-key (kbd "C-<tab>") 'completion-at-point)


;; TODO: May not want to use this. Might encourage bad habits.
;; Move more quickly
(global-set-key (kbd "C-S-n") (lambda () (interactive) (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (lambda () (interactive) (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (lambda () (interactive) (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (lambda () (interactive) (ignore-errors (backward-char 5))))


;; Comment/uncomment block
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c u") 'uncomment-region)

;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

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

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; Browse the kill ring
(global-set-key (kbd "C-x C-y") 'browse-kill-ring)


;; Expand region (by semantic units)
(global-set-key (kbd "C-@") 'er/expand-region) ; don't like this keybinding.

;; TODO: Read cursor documentation, and find keybindings for these bad boys.
;; Experimental multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-S-c C-e") 'mc/edit-ends-of-lines)
;; (global-set-key (kbd "C-S-c C-a") 'mc/edit-beginnings-of-lines)

;; ;; Mark additional regions matching current region
;; (global-set-key (kbd "M-æ") 'mc/mark-all-like-this-dwim)
;; (global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-æ") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-Æ") 'mc/mark-more-like-this-extended)
;; (global-set-key (kbd "M-å") 'mc/mark-all-in-region)

;; ;; Symbol and word specific mark-more
;; (global-set-key (kbd "s-æ") 'mc/mark-next-word-like-this)
;; (global-set-key (kbd "s-å") 'mc/mark-previous-word-like-this)
;; (global-set-key (kbd "M-s-æ") 'mc/mark-all-words-like-this)
;; (global-set-key (kbd "s-Æ") 'mc/mark-next-symbol-like-this)
;; (global-set-key (kbd "s-Å") 'mc/mark-previous-symbol-like-this)
;; (global-set-key (kbd "M-s-Æ") 'mc/mark-all-symbols-like-this)
;; 
;; ;; Extra multiple cursors stuff
;; (global-set-key (kbd "C-~") 'mc/reverse-regions)
;; (global-set-key (kbd "M-~") 'mc/sort-regions)
;; (global-set-key (kbd "H-~") 'mc/insert-numbers)

(provide 'keybindings)
