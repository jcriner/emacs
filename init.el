;;;; .emacs.d/init.el, Jacob Criner
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

;;;----------------------------------------
;;; Preliminaries

;; Turn off mouse interface early on in startup to avoid momentary display
;(if (fboundp 'menu-bar-mode) (menu-bar-mode -1)) 
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Disable splash screen at start-up.
(setq inhibit-splash-screen t)

;; Set path to dependencies
(setq site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(setq user-elisp-dir (concat user-emacs-directory "elisp"))

;; Set up load path
(add-to-list 'load-path user-elisp-dir)
(add-to-list 'load-path site-lisp-dir)

;; Machine-specific settings
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

;; Emacs server
(require 'server)
(unless (server-running-p)
  (server-start))

;;;--------------------------------------------
;;; Package management.

;; Only include the MELPA archive.
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

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
;;
;; TODO: Cleanout most of these packages. I use very few of them.
(defvar my-packages '(
                      diminish
                      evil ; bring on the Vim stuff
                      ido-ubiquitous
                      professional-theme
                      smooth-scrolling
                      undo-tree
                      )
  "A list of packages to ensure are installed at launch.")


;; Install packages that are not present on current machine.
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))) ; Finds package installed in package-archives

;;;----------------------------------------
;;; Requires, Loads, etc.

(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-M-x-fuzzy-match t
          helm-buffers-fuzzy-matching t)
    (helm-mode))
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring))
  :bind (:map helm-map
             ("<tab>" . helm-execute-persistent-action)
              ("C-z"   . helm-select-action)))
;; TODO: Switch out of ido-mode and similar completely.
;;
;; Pain points with Helm: file organization is terrible when searching
;; for files (presents dot-files first, for instance). Also has no
;; intelligence, e.g. sorting most used files near the top.

(use-package helm-swoop
  :ensure t
  ;; TODO: Pick a keybinding for this, and for
  ;; helm-multi-swoop, etc.
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

;; Setup Yasnippet.
(use-package yasnippet
  :ensure t
  :init
  (progn
    (setq yas/snippet-dirs '("~/.emacs.d/snippets")
          yas-installed-snippets-dir ""
          yas/wrap-around-region t
          yas/expand-only-for-last-commands '(self-insert-command
                                              yas-exit-all-snippets
                                              yas-abort-snippet
                                              yas-skip-and-clear-or-delete-char
                                              yas-exit-field-or-maybe-expand)))
  :config
  (progn
    (yas/global-mode t)
    (define-key yas/keymap (kbd "<return>") 'yas-exit-all-snippets)))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Requires for my personal files
(require 'keybindings) ; Setup key bindings
(require 'sane-defaults) ; Paren-matching, active region, etc.
;; Misc
; (require 'misc) ; This is where 'zap-up-to-char comes from.

;; GUI-specific Emacs configuration
;;
;; Note: This will not handle the spawning of new frames. A better way
;; to handle this is actually to encapsulate this in a function called
;; by the after-frame-created-hook.
(when (display-graphic-p)
  ;; Theme loaded here due to bugs loading elsewhere...
  ;; Note: DejaVu may be more readable. It has much better Unicode support, as well.
  ;; (set-default-font "DejaVu Sans Mono-10")
  (set-default-font "Anonymous Pro-11"))
(load-theme 'professional t)

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

;;;--------------------------------------------------
;;; Load user specific stuff

;; Conclude init by setting up specifics for the current machine
(when (file-exists-p machine-settings-dir)
  (mapc 'load (directory-files machine-settings-dir nil "^[^#].*el$"))) 
                                        ; Note that this ignores .elc
                                        ; files. May want to revise
                                        ; this regexp slightly.

;;;--------------------------------------------------
;;; Miscellaneous

;; Turn off atrocious scroll acceleration and such.
(setq mouse-wheel-scroll-amount '(3))
(setq mouse-wheel-progressive-speed nil)

;; Set fill-column to 80, as God intended. (Previous default: 70)
(setq-default fill-column 80)
