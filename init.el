;;
;; Visuals
;;

(tool-bar-mode 0)
(menu-bar-mode 1)
(scroll-bar-mode 0)

;; show cwd in tile bar
(setq frame-title-format '((:eval default-directory)))

;; Saner defaults
(global-auto-revert-mode 1) ;; auto load file changes
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto load dired changes
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t) ;; disable splash screen

(setq compilation-scroll-output t) ;; will automatically place cursor at end of compilation buffer on first compile

(setq-default show-trailing-whitespace t)

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
  (infer-indentation-style))

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'setup-general-code-modes))

;; Term mode hook
(defun fn-term-mode-hook ()
  (setq show-trailing-whitespace nil))
(add-hook 'term-mode-hook 'fn-term-mode-hook)

;;
;; Key binds
;;

(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)

(global-set-key (kbd "C-x t") 'delete-trailing-whitespace)

(global-set-key (kbd "C-x G") 'compile)
(global-set-key (kbd "C-x C-g") 'recompile)

(global-set-key (kbd "C-x w") 'whitespace-mode)

(global-set-key (kbd "C-j") (lambda () (interactive) (join-line -1)))

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
        rg
        evil
        undo-tree
        rainbow-delimiters
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
;; Simple utility
;;

(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(require 'projectile)
(global-set-key (kbd "C-x p") 'projectile-command-map)

(require 'magit)

(require 'rg)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'which-key)
(which-key-mode)

(require 'undo-tree)
(global-undo-tree-mode)

;;
;; Smooth scrolling
;;

;; imporved mouse scrolling
;; scroll one line at a time (less "jumpy" than defaults)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;;
;; Evil mode
;;

(require 'evil)
(evil-mode 1)

(setq evil-default-cursor '("green" box)
      evil-normal-state-cursor '("green" box)
      evil-emacs-state-cursor '("red" box)
      evil-insert-state-cursor '("yellow" bar))

;;
;; LSP stuff
;;

(require 'lsp-mode)
(require 'lsp-ui)
(require 'company)
(require 'company-lsp)

(require 'lsp)

(setq lsp-ui-doc-enable nil)

(define-key evil-normal-state-map (kbd "C-d") 'lsp-find-definition)

(global-set-key (kbd "C-x x d") 'lsp-find-definition)
(global-set-key (kbd "C-x x n") 'lsp-rename)
(global-set-key (kbd "C-x x r") 'lsp-format-region)
(global-set-key (kbd "C-x x b") 'lsp-format-buffer)
(global-set-key (kbd "C-x x k") 'lsp-ui-doc-glance)

(add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection "zls")
  :major-modes '(zig-mode)
  :server-id 'zls))

(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'zig-mode-hook 'lsp)
