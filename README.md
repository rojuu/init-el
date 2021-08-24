# init-el

Emacs config for my coding stuff when I feel like it. Intended to be cloned separatedy and loaded in emacs init.el file. That way you can also add other configurations for each computer. Example `~/.emacs.d/init.el` file:
```
(load "~/Projects/init-el/init.el")

;; optionally set font
;; (set-frame-font "Inconsolata 13" nil t nil)

;; optionally set eglot mode for specific languages (not installed by default)
;; (add-hook 'cc-mode-hook 'eglot)

(setq custom-safe-themes t)   ; Treat all themes as safe
(load-theme 'inkpot)
```

For project specific config you can for example use this `.dir-locals.el` file that has been useful with this config for C++:
```
;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((c++-mode
  (projectile-project-compilation-cmd . "cmake --build Debug")
  (projectile-project-run-cmd . "cd ..; build/Debug/<program_name>")
  (projectile-project-test-cmd . "cd Debug; ./tests/<test_name>")
  (eval add-hook 'before-save-hook #'lsp-format-buffer)))

```
This will adjust projectile build commands to be better and autoformat the buffer with `lsp-format-buffer` when saving the buffer.
The local config assumes a following folder structure:
```
project-name:
    * src
      * some.cpp
      * some.hpp
    * build
      * Debug
        * <prog_name>
        * tests
          * <test_name>
    README.md
```

