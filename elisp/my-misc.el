;;;; my-misc.el
;;;;
;;;; Location of some random stuff.


;; Seed the random-number generator
(random t)

;; TODO: Don't know what settings this creates.
;; Whitespace-style
; (setq whitespace-style '(trailing lines space-before-tab
;                                   indentation space-after-tab)
;       whitespace-line-column 100)

;; TODO: This isn't a function: 'cleanup-buffer-safe' in default Emacs
;; Various superfluous white-space. Just say no.
;(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; A bit of misc cargo culting in misc.el
(setq xterm-mouse-mode t)


(provide 'my-misc)
