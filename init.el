;;
;; Visuals
;;
(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)

(setq-default cursor-type 'bar)

;; Saner defaults
(global-auto-revert-mode 1) ;; auto load file changes
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto load dired changes
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)

(setq inhibit-startup-message t) ;; disable splash screen

;;
;; Indentation and code style
;;

(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(defun infer-indentation-style ()
  (setq-default indent-tabs-mode nil)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode (set to nil by default)
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun setup-general-code-modes ()
  (infer-indentation-style)
  (setq-default show-trailing-whitespace t))

(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook 'setup-general-code-modes))

;;
;; Key binds
;;

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x t") 'delete-trailing-whitespace)

(global-set-key (kbd "C-,") (kbd "C-x @ s"))

(global-set-key (kbd "C-x G") 'compile)
(global-set-key (kbd "C-x C-g") 'recompile)

;;
;; Packages
;;

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
		("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq my-packages
      '(projectile
        ido
        magit
        sublimity
        which-key
        lsp-mode
        lsp-ui
        company
        company-lsp
		zig-mode
        markdown-mode
        json-mode))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;
;; Fuzzy find and file opening
;;

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'projectile)
(global-set-key (kbd "C-x p") 'projectile-find-file)

(require 'magit)

;;
;; Smooth scrolling
;;

;; imporved mouse scrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

(require 'sublimity)
(require 'sublimity-scroll)
(sublimity-mode 1)

;; Which key
(require 'which-key)
(which-key-mode)

;;
;; LSP stuff
;;

(require 'lsp-mode)
(require 'lsp-ui)
(require 'company)
(require 'company-lsp)

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

