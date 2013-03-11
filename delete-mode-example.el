;; A scratch buffer with some ideas for semi-modal editing of text.
;;
;; The idea applies to many things:
;; * A prefix command for a mode (such as C-d for "deletion mode")
;; * Consistent application of certain chars for various levels of
;;   textual expression.

(global-unset-key (kbd "C-d"))

(global-set-key (kbd "C-d d") 'delete-forward-char)
(global-set-key (kbd "C-d w") 'subword-kill)
(global-set-key (kbd "C-d l") 'delete-line)

;; Redudancy
(global-set-key (kbd "C-d C-d") 'delete-forward-char)
(global-set-key (kbd "C-d C-w") 'subword-kill)
(global-set-key (kbd "C-d C-l") 'delete-line)

;; Similarly for expressions, buffers, regions.

(defun delete-line ()
  (interactive)
  (beginning-of-line)
  (kill-line))

;; Then add something similar for repetition:
;;
;; If last command was 'delete-forward-char, then continuing to tap to
;; C-d would continue to delete forward characters.
;;
;; In fact, why not make it it's own little tiny mode. You press C-d,
;; and you're in delete mode, so any time you press d, w, l,
;; e(xpression), or whatever else while you're deleting, you get
;; instant response.



