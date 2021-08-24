;;(require 'benchmark-init)
;;;;To disable collection of benchmark data after init is done.
;;(add-hook 'after-init-hook 'benchmark-init/deactivate)

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(cua-mode 0)

(setq make-pointer-invisible t)

(setq frame-title-format '((:eval default-directory)))

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

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t)

(setq compilation-scroll-output t) ;; will automatically place cursor at end of compilation buffer on first compile

;;(setq-default show-trailing-whitespace t)
(setq-default show-trailing-whitespace nil)

(setq transient-mark-mode nil)

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

(defun fn-term-mode-hook ()
  (setq show-trailing-whitespace nil))
(add-hook 'term-mode-hook 'fn-term-mode-hook)


(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(global-set-key [M-left]  'backward-global-mark)
(global-set-key [M-right] 'forward-global-mark)

(global-set-key [home] 'back-to-indentation)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))    


(global-set-key (kbd "M-i") 'ido-goto-symbol)

(global-set-key (kbd "C-,") 'other-window)
(global-set-key (kbd "C-.") 'split-window-right)
(global-set-key (kbd "C-:") 'delete-window)
(global-set-key (kbd "C-;") 'delete-other-windows)

(global-set-key (kbd "C-x C-g") 'recompile)

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

(use-package ido
  :ensure t
  :config
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (ido-mode 1))

(use-package subr-x)

(use-package projectile
  :ensure t
  :config
  (global-set-key (kbd "C-x p") 'projectile-command-map)
  (global-set-key (kbd "M-p") 'projectile-find-file)
  (global-set-key (kbd "M-o") 'projectile-find-other-file)
  (global-set-key (kbd "C-S-s") 'projectile-ripgrep)
  (projectile-global-mode))


(use-package magit
  :ensure t
  :defer t)

(use-package rg
  :ensure t
  :defer t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook (lambda () (rainbow-delimiters-mode t))))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package inkpot-theme
  :ensure t)

(use-package ujelly-theme
  :ensure t)


;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 8) ((control) . nil))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;;
;; Language modes
;;

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

