;;;; sane-defaults.el
;;;;
;;;; Handles the setting of important defaults, such as allowing
;;;; pasting selections from outside of Emacs, not using tabs, setting
;;;; default column width, and so on.

;; For anything that uses tabs, let's have reasonable tab size.
(setq-default tab-width 2)

;; Highlight matching parens.
(show-paren-mode t)

;; And auto-pair delimiters
(electric-pair-mode t)

;; Show a few more lines of context with each page scroll.
(setq next-screen-context-lines 5)

;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Point exists /between/ characters in insert mode, over in overwrite mode.
(set-default 'cursor-type 'bar)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Column filling defaults to 80 chars on a line.
(set-fill-column 70)

;; TODO: Need to understand what this does better.
;; Move files to trash when deleting
;(setq delete-by-moving-to-trash t)

;; Don't need to use shift to mark things.
;; This frees up S-<arrow keys>, etc, as well.
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Make all indentation spaces-only. Tabs shall not pass!
(setq-default indent-tabs-mode nil)

;; Overwrite active region when inserting text.
(delete-selection-mode t)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Set to 't' to stop from breaking lines.
(setq-default truncate-lines t)

;; ;; Keep cursor away from edges when scrolling up/down
;; (require 'smooth-scrolling)

;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and
;; M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Fontify org-mode code blocks
(setq org-src-fontify-natively t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Add parts of each file's directory to the buffer name if not unique
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)


;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region cruft.
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (when (eq last-command 'save-region-or-current-line)
      ad-do-it
      ad-do-it
      ad-do-it)
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))

(provide 'sane-defaults)

;;; sane-defaults.el ends here
