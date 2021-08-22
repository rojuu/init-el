;;(require 'benchmark-init)
;; To disable collection of benchmark data after init is done.
;;(add-hook 'after-init-hook 'benchmark-init/deactivate)

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

;; auto repeat search
(defadvice isearch-repeat (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-repeat 'after 'isearch-no-fail)
    (ad-activate 'isearch-repeat)))

(setq ring-bell-function 'ignore)

(setq inhibit-startup-message t) ;; disable splash screen

(setq compilation-scroll-output t) ;; will automatically place cursor at end of compilation buffer on first compile

;;(setq-default show-trailing-whitespace t)
(setq-default show-trailing-whitespace nil)

(setq transient-mark-mode nil)


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


;; Back/forward withing jump list
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

;;
;; Packages
;;

(setq package-archives
      '(("gnu"          . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa"        . "http://melpa.org/packages/")))

(require 'package)
(package-initialize)

(setq my-packages
      '(projectile
        ido
        magit
        rg
        dumb-jump
        evil
        ;; undo-tree
        rainbow-delimiters
        which-key
        ;; lsp-mode
        ;; lsp-ui
        ;; company
        ;; company-lsp
        inkpot-theme
        ujelly-theme
        zig-mode
        markdown-mode
        json-mode
        glsl-mode
        ))

(unless package-archive-contents
  (package-refresh-contents))

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

(require 'dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

(require 'which-key)
(which-key-mode)

;; (require 'undo-tree)
;; (global-undo-tree-mode)

;;
;; Smooth scrolling
;;

;; imporved mouse scrolling
;; scroll one line at a time (less "jumpy" than defaults)
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 8) ((control) . nil))) ;; one line at a time
;; (setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
;; (setq scroll-step 1) ;; keyboard scroll one line at a time

;;
;; Language modes
;;

(autoload 'glsl-mode "glsl-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
(add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))

;;
;; Evil mode
;;

(require 'evil)
(evil-mode 1)
;; (setq evil-default-cursor '("green" box)
;;       evil-normal-state-cursor '("green" box)
;;       evil-emacs-state-cursor '("red" box)
;;       evil-insert-state-cursor '("yellow" bar))

;;
;; Key binds
;;

;; (global-set-key (kbd "C-x G") 'compile)
(global-set-key (kbd "C-x C-g") 'recompile)

(global-set-key (kbd "C-x w") 'whitespace-mode)

(global-set-key (kbd "C-o") 'backward-global-mark)
(global-set-key (kbd "C-i") 'forward-global-mark)
(global-set-key (kbd "<mouse-8>") 'backward-global-mark)
(global-set-key (kbd "<mouse-9>") 'forward-global-mark)

(global-set-key (kbd "C-,") 'other-window)

(global-set-key (kbd "M-i") 'ido-goto-symbol)

;;
;; LSP stuff
;;

;; (require 'lsp-mode)
;; (require 'lsp-ui)
;; (require 'company)
;; (require 'company-lsp)

;; (require 'lsp)

;; (setq lsp-ui-doc-enable nil)

;; (define-key evil-normal-state-map (kbd "C-d") 'lsp-find-definition)

;; (global-set-key (kbd "<f12>") 'lsp-find-definition)
;; (global-set-key (kbd "C-x x d") 'lsp-find-definition)
;; (global-set-key (kbd "C-x x n") 'lsp-rename)
;; (global-set-key (kbd "C-x x r") 'lsp-format-region)
;; (global-set-key (kbd "C-x x b") 'lsp-format-buffer)
;; (global-set-key (kbd "C-x x k") 'lsp-ui-doc-glance)

;; (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
;; (lsp-register-client
;; (make-lsp-client
;;  :new-connection (lsp-stdio-connection "zls")
;;  :major-modes '(zig-mode)
;;  :server-id 'zls))

;; (add-hook 'c-mode-hook 'lsp)
;; (add-hook 'c++-mode-hook 'lsp)
;; (add-hook 'zig-mode-hook 'lsp)
