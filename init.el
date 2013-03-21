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


;; Electric-Pair-Mode, may want to enable and configure that.


;; Turn off mouse interface early on in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen at start-up.
(setq inhibit-splash-screen t)

;; Appearance: (Set early on to avoid 'flicker' of changing colors, etc)

;; Note: DejaVu may be more readable. It has much better Unicode support, as well.
;; (set-default-font "DejaVu Sans Mono-10")
(set-default-font "Anonymous Pro-11")


;; For some reason, this 'load-theme doesn't work here, and is placed
;; later on in the file.
;;
;; (load-theme 'zenburn t)

;;----------------------------------------

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
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

(require 'setup-package)

;; List of packages I expect to have anywhere.
;;
;; Note: use of 'defvar' means that updating this list and then
;; eval-ing the buffer won't actually update the my-packages var. May
;; want to adopt a different idiom for this.

(defvar my-packages '(
                      ace-jump-mode
                      auctex
                      dash ; dep for dired functionality
                      dired-details
                      ; erlang ; Currently, package is broken by flymake (orphaned package)
                      expand-region ; TODO: buggy, right now?
                      haskell-mode
                      ido-ubiquitous
                      iy-go-to-char
                      key-chord ; TODO: Need to configure. Badass, though.
                      multiple-cursors ; TODO: need to configure
                      paredit
                      paredit-menu
                      smex
                      smooth-scrolling
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

;; ido-config
(require 'setup-ido)


;; Setup extensions
(eval-after-load 'dired '(require 'setup-dired))

;; Language-specific setup files
(require 'setup-lisp)

;; YASnippet configuration.
(require 'setup-yasnippet)

;; Map files to modes
(require 'mode-mappings)

;; Misc
(require 'misc) ; This is where 'zap-up-to-char comes from.
(require 'my-misc)
; Appearance settings are also misc, too.


;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))



;; Diminish modeline clutter (maybe refactor to setup-* files)
(require 'diminish)
;; TODO: Need to debug these.
;(diminish 'paredit-mode)
;(diminish 'eldoc-mode)


;; Browse kill ring
(require 'browse-kill-ring)
(setq browse-kill-ring-quit-action 'save-and-restore)


;; Smart M-x is smart
(require 'smex)
(smex-initialize)


;; iy-go-to-char
(require 'iy-go-to-char)

;; Setup key bindings
(require 'keybindings)

;; Theme loaded here due to bugs loading elsewhere...
(load-theme 'zenburn t)

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))


;;--------------------------------------------------
;; Load user specific stuff

;; Conclude init by setting up specifics for the current user
(when (file-exists-p machine-settings-dir)
  (mapc 'load (directory-files machine-settings-dir nil "^[^#].*el$"))) 
                                        ; Note that this ignores .elc
                                        ; files. May want to revise
                                        ; this regexp slightly.
(put 'narrow-to-region 'disabled nil)
