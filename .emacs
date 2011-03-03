(add-to-list 'load-path "~/.emacs.d/plugins")

(setq inhibit-startup-message t)

(tool-bar-mode nil)
(scroll-bar-mode nil)
(show-paren-mode t)

(if (eq window-system 'x)
    (progn
      (set-default-font "Monaco-8")
      (require 'color-theme)
      (color-theme-blue-mood)))

(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 1)))

(global-set-key [(f7)] 'compile)
(global-set-key [(f12)] 'eshell)

(defun elisp-mode-hook ()
  (local-set-key [(meta return)] 'lisp-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

(defun c/c++-mode-hook ()
  (c-set-style "linux"))
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippets")
