(setq inhibit-startup-message t)

(tool-bar-mode nil)
(scroll-bar-mode nil)
(show-paren-mode t)

(if (eq system-type 'gnu/linux)
    (set-default-font "Monaco-8"))

(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 1)))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 1)))

(defun elisp-mode-hook ()
  (local-set-key [(tab)] 'lisp-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

(defun c/c++-mode-hook ()
  (c-set-style "linux"))
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/yasnippets")
