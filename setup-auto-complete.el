;;; setup-auto-complete.el
;;;
;;; Configure auto-completion using the auto-complete package.

(require 'auto-complete)

;; Turn it on.
(auto-complete-mode)

;; Configuration, settings.
(require 'auto-complete-config)
(ac-config-default)

(provide 'setup-auto-complete)
;;; -- setup-auto-complete.el ends here
