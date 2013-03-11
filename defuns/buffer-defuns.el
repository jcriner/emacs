;;;; buffer-defuns.el
;;;;
;;;; Functions related to buffer manipulatio

(defun split-window-right-and-move-there-dammit ()
  (interactive)
  (split-window-right)
  (windmove-right))
