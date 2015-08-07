;;;; .emacs.d/init.el, Jacob Criner
;;;; 
;;;; Selections of this file were taken from Magnar Sveen's .emacs.d/
;;;;
;;;; For standard elisp dev, C-M-x for eval-defun. C-u in front to enable edebug.

;;;--------------------------------------------------
;;;
;;; Principles of Editors: A Friendly Reminder
;;; (a.k.a. A Unified Field Theory of Editors)
;;; auth: jcriner
;;; date: 2/24/13
;;;
;;; Categories of internal behavior for a text editor:
;;; * Editing: surgical insertion, deletion, and alteration of text
;;; * Navigation: moving within a file, between buffers/files, searching
;;; * Code generation: auto-completion, code snippets/templates
;;; * Documentation: fetch language/library docs easily
;;;
;;; Additionally, automatic behavior:
;;; * Functions attached to mode hooks; pre-commit test-runs, etc
;;;
;;; External access:
;;; * Shells: Unix shells, language REPLs
;;; * Compilation: easily compile and run code
;;; * Version control: repo interaction
;;;
;;;--------------------------------------------------


;; Turn off mouse interface early on in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen at start-up.
(setq inhibit-splash-screen t)

;; Appearance: (Set early on to avoid 'flicker' of changing colors, etc)

;; For some reason, this 'load-theme doesn't work here, and is placed
;; later on in the file.
;;
;; (load-theme 'zenburn t)

;;----------------------------------------

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

(setq user-elisp-dir (concat user-emacs-directory "elisp"))

;; Set up load path
(add-to-list 'load-path user-elisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; User specific settings (make machine specific?)
(setq machine-settings-dir
      (concat user-emacs-directory "machine-specific/"))
(add-to-list 'load-path machine-settings-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)


;; ;; Save point position between sessions
;; (require 'saveplace)
;; (setq-default save-place t)
;; (setq save-place-file (expand-file-name ".places" user-emacs-directory))

;;;--------------------------------------------
;;; Package management.
;;;
;;; Magnar Sveen's .emacs.d/ does this differently, but I'm not
;;; convinced his approach is actually better.

;; Require 'package, initialize it, and ensure ELPA, MELPA, and
;; Marmalade are available repos.
(require 'setup-package)

;; TODO: Convert existing files to use `use-package' exclusively.
;;
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile 
  (require 'use-package))
(require 'bind-key)

;; List of packages I expect to have anywhere.
;;
;; Note: use of 'defvar' means that updating this list and then
;; eval-ing the buffer won't actually update the my-packages var. May
;; want to adopt a different idiom for this.

(defvar my-packages '(
                      ace-jump-mode
                      change-inner ; emulate vim's ci and co commands
                      dash
                      diminish
                      erlang
                      expand-region
                      evil ; bring on the Vim stuff
                      haskell-mode
                      ido-ubiquitous
                      iy-go-to-char
                      ; js2-mode ; improved javascript editing
                      keyfreq
                      magit ; Git management from Emacs.
                      paredit
                      paredit-menu
                      professional-theme
                      projectile
                      smooth-scrolling
                      unbound ; conveniently list unbound keys
                      undo-tree
                      yasnippet
                      zenburn-theme
                      )
  "A list of packages to ensure are installed at launch.")


;; Install packages that are not present on current machine.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) ; Finds package installed in package-archives

;;;----------------------------------------
;;; Requires, Loads, etc.

;; Paren-matching, active region, etc.
(require 'sane-defaults)

;; company config
(use-package company
  :ensure t
  :config
  (progn
    (global-company-mode t)))

(use-package company-quickhelp
  :ensure t
  :config
  (progn
    (company-quickhelp-mode t)))

(use-package company-ycmd
  :ensure t
  :config
  (progn
    (add-to-list 'company-backends 'company-ycmd)))

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-buffers-fuzzy-matching t)
    (helm-mode))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)))
;; TODO: Switch out of ido-mode and similar completely.

(use-package helm-swoop
  :ensure t
  ;; TODO: Pick a keybinding for this, and for
  ;; helm-multi-swoop, etc.
)

;; TODO: Write a setup config for projectile.
(use-package projectile
  :ensure t
  :defer t
  :diminish projectile-mode
)


;; ido-config
(use-package ido
  :ensure ido-ubiquitous
  :init
  (progn
    (require 'ido)
    (ido-mode t)
    (setq ido-enable-prefix nil
          ido-enable-flex-matching t
          ido-auto-merge-work-directories-length nil
          ido-create-new-buffer 'always
          ido-use-filename-at-point 'guess
          ido-use-virtual-buffers t
          ido-handle-duplicate-virtual-buffers 2
          ido-max-prospects 10)
    (require 'ido-ubiquitous)
    (ido-ubiquitous-mode t))
  :config
  (progn
    (add-hook 'ido-setup-hook
              (lambda ()
                (define-key ido-file-completion-map
                  (kbd "~")
                  (lambda ()
                    (interactive)
                    (cond
                     ((looking-back "/") (insert "~/"))
                     (:else (call-interactively 'self-insert-command)))))))
    (require 'ido-ubiquitous)
    (ido-ubiquitous-mode t)))

;; Setup extensions
(eval-after-load 'dired '(require 'setup-dired))

;; Language-specific setup files
(require 'setup-lisp)

;; YASnippet configuration.
(require 'setup-yasnippet)

;; Map files to modes
(require 'mode-mappings)

;; List unbound key sequences.
(require 'unbound)

;; Misc
(require 'misc) ; This is where 'zap-up-to-char comes from.
(require 'my-misc)
; Appearance settings are also misc, too.

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; iy-go-to-char
(require 'iy-go-to-char)

;; Setup key bindings
(require 'keybindings)

;; Keep track of my command usage.
;; This information is intended to be used to:
;; * Determine efficient keybindings
;; * Find inefficiencies in my editing
;; * Investigate further movement towards Evil.
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)


;; GUI-specific Emacs configuration
;;
;; Note: This will not handle the spawning of new frames. A better way
;; to handle this is actually to encapsulate this in a function called
;; by the after-frame-created-hook.
(when (display-graphic-p)
  ;; Theme loaded here due to bugs loading elsewhere...
  ;;;;;; (load-theme 'zenburn t)
  ;; Note: DejaVu may be more readable. It has much better Unicode support, as well.
  ;; (set-default-font "DejaVu Sans Mono-10")
  (set-default-font "Anonymous Pro-11"))
; (load-theme 'zenburn t)
(load-theme 'professional t)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))


(use-package multiple-cursors
  :ensure t
  :bind (("C-S-c C-S-c"   . mc/edit-lines)
         ("C-S-c C-e"     . mc/edit-ends-of-lines)
         ("C-S-c C-a"     . mc/edit-beginnings-of-lines)
         ("C->"           . mc/mark-next-like-this)
         ("C-<"           . mc/mark-previous-like-this)
         ("C-<return>"    . mc/mark-more-like-this-extended)
         ("C-S-SPC"       . set-rectangular-region-anchor)
         ("C-M-="         . mc/insert-numbers)
         ("C-*"           . mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))


(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
         ("M-o" . change-outer)))

;;;--------------------------------------------------
;;; Load user specific stuff

;; Conclude init by setting up specifics for the current user
(when (file-exists-p machine-settings-dir)
  (mapc 'load (directory-files machine-settings-dir nil "^[^#].*el$"))) 
                                        ; Note that this ignores .elc
                                        ; files. May want to revise
                                        ; this regexp slightly.

;; TODO: enabled all disabled commands.
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;;--------------------------------------------------
;;; Temporary dumping ground.

;; Toggle selective-display based on where the cursor is.
(defun toggle-selective-display-at-point ()
  (interactive)
  (set-selective-display
   (if selective-display
       nil
     (+ 1 (current-column)))))

;; Override the original 'selective-display' keybinding.
(global-set-key (kbd "C-x $") 'toggle-selective-display-at-point)
(put 'dired-find-alternate-file 'disabled nil)
(put 'upcase-region 'disabled nil)
