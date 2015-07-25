;;; Created Sun. Feb. 3rd
;;; Auth: Jacob Criner

;; Moves to beginning of indentation on first call, then beginning of
;; line on second. Does nothing if called more than twice.
(defun smart-beginning-of-line () 
  (interactive)
  (if (eq last-command 'smart-beginning-of-line)
      (move-beginning-of-line 1)
    (back-to-indentation)))

