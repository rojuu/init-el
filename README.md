# init-el

Emacs config for my coding stuff when I feel like it. Intended to be cloned separatedy and loaded in emacs init.el file. That way you can also add other configurations for each computer. Example `~/.emacs.d/init.el` file:
```
(setq default-frame-alist '((font . "Fira Mono-14")))
(load-theme 'tango-dark)

(load "~/Projects/init-el/init.el")
```
