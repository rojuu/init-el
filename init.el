;; (require 'benchmark-init)
;; (add-hook 'after-init-hook 'benchmark-init/deactivate)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(cua-mode 0)

(setq make-pointer-invisible t)
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

(setq frame-title-format '((:eval default-directory)))

;;(setq-default cursor-type '(bar . 1))
(setq-default cursor-type 'box)
(setq-default cursor-in-non-selected-windows nil)
(set-cursor-color "green")

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(global-auto-revert-mode 1) ;; auto load file changes
(add-hook 'dired-mode-hook 'auto-revert-mode) ;; auto load dired changes
(defalias 'yes-or-no-p 'y-or-n-p)
(setq make-backup-files nil)
(setq auto-save-default nil)

(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

(setq-default truncate-lines t)
(setq truncate-partial-width-windows nil)
;; (defun my-compilation-hook ()
;;   (make-local-variable 'truncate-lines)
;;   (setq truncate-lines nil)
;; )
;; (add-hook 'compilation-mode-hook 'my-compilation-hook)

(setq compilation-scroll-output t) ;; will automatically place cursor at end of compilation buffer on first compile

;;(setq-default show-trailing-whitespace t)
(setq-default show-trailing-whitespace nil)

(setq transient-mark-mode nil)

(setq-default tab-width 4)
(setq-default c-basic-offset 4)

(setq electric-indent-inhibit t)
(setq backward-delete-char-untabify-method 'untabify)

(defun enable-tabs ()
  ;; make tab insert a tab when indenting with tabs y'know
  ;;(local-set-key (kbd "TAB") 'tab-to-tab-stop)
  (setq c-indent-tabs-mode t)
  (setq indent-tabs-mode t))

(defun enable-spaces ()
  (setq indent-tabs-mode nil))

(defun infer-indentation-style ()
  (setq-default indent-tabs-mode nil)
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode (set to nil by default)
  (let ((space-count (how-many "^  " (point-min) (point-max)))
        (tab-count (how-many "^\t" (point-min) (point-max))))
    (if (> space-count tab-count) (enable-spaces))
    (if (> tab-count space-count) (enable-tabs))))

(setq-default tab-always-indent nil)
(setq-default c-tab-always-indent nil)

(defun setup-general-code-modes ()
  ;; Disable the annoying electric indent shit when I type a ; at the end of a statement
  (define-key c-mode-base-map "#" 'self-insert-command)
  (define-key c-mode-base-map "(" 'self-insert-command)
  (define-key c-mode-base-map "*" 'self-insert-command)
  (define-key c-mode-base-map "," 'self-insert-command)
  (define-key c-mode-base-map "/" 'self-insert-command)
  (define-key c-mode-base-map ":" 'self-insert-command)
  (define-key c-mode-base-map ";" 'self-insert-command)
  (define-key c-mode-base-map "{" 'self-insert-command)
  (define-key c-mode-base-map "}" 'self-insert-command)
  (infer-indentation-style))

(dolist (hook '(prog-mode-hook))
  (add-hook hook 'setup-general-code-modes))

(defun fn-term-mode-hook ()
  (setq show-trailing-whitespace nil))
(add-hook 'term-mode-hook 'fn-term-mode-hook)


(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))


(global-set-key [M-left]  'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up]    'windmove-up)
(global-set-key [M-down]  'windmove-down)

(defun my-split-window-left ()
  ""
  (interactive)
  (split-window-right))
(defun my-split-window-right ()
  ""
  (interactive)
  (split-window-right)
  (windmove-right))
(defun my-split-window-up ()
  ""
  (interactive)
  (split-window-vertically)
  (windmove-up))
(defun my-split-window-down ()
  ""
  (interactive)
  (split-window-below)
  (windmove-down))

(global-set-key [C-M-left]  'my-split-window-left)
(global-set-key [C-M-right] 'my-split-window-right)
(global-set-key [C-M-up]    'my-split-window-up)
(global-set-key [C-M-down]  'my-split-window-down)

(global-set-key (kbd "C-z") 'recenter-top-bottom)


(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-.") 'my-split-window-right)
(global-set-key (kbd "C-q") 'delete-window)
(global-set-key (kbd "C-:") 'delete-window)
(global-set-key (kbd "C-;") 'delete-other-windows)

(global-set-key (kbd "C-x c") 'recompile)
(global-set-key (kbd "C-x q") 'quick-calc)
(global-set-key (kbd "C-x g") 'goto-line)

(global-set-key (kbd "C-<return>") 'dabbrev-expand)

(global-set-key (kbd "C-j") 'top-join-line)

(require 'misc)
(global-set-key (kbd "M-z") 'zap-up-to-char)

;; + and \ are on same key on nordic layout, so emulate M-\ with these
(global-set-key (kbd "M-+") 'delete-horizontal-space)
(global-set-key (kbd "C-M-+") 'indent-region)


;;
;; Stuff that requires packages
;;

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq my-packages
      '(use-package))

(unless package-archive-contents
  (package-refresh-contents))

(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(require 'use-package)

;; Adds a folder called "modes" relative to this file into the load-path.
;; All my custom modes are in the modes folder
(add-to-list 'load-path (expand-file-name "modes/" (file-name-directory load-file-name)))


(use-package ido
  :ensure t
  :defer t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  :init
  (ido-mode 1))

(use-package subr-x)

(use-package projectile
  :ensure t
  :defer t
  :bind (("C-x p" . projectile-command-map)
         ("M-p" . projectile-find-file)
         ("M-o" . projectile-find-other-file)
         ("C-S-s" . projectile-ripgrep))
  :init
  (projectile-global-mode))

(use-package rg
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :defer t
  :config
  (setq dumb-jump-prefer-searcher 'rg)
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . (lambda() (rainbow-delimiters-mode t))))

(use-package which-key
  :ensure t
  :defer t
  :init
  (which-key-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(setq custom-safe-themes t)   ; Treat all themes as safe

(use-package jbeans-theme
  :ensure t
  :defer t
  :init
  (load-theme 'jbeans))

;;
;; Language modes
;;

(require 'hlsl-mode)

(use-package glsl-mode
  :ensure t
  :defer t
  :config
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode)))

(use-package zig-mode
  :defer t
  :ensure t)

(use-package cmake-mode
  :defer t
  :ensure t)

(use-package markdown-mode
  :defer t
  :ensure t)

(use-package json-mode
  :defer t
  :ensure t)

(use-package glsl-mode
  :defer t
  :ensure t)

(use-package protobuf-mode
  :defer t
  :ensure t)

(use-package typescript-mode
  :defer t
  :ensure t)

