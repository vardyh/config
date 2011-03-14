(setq config-dir "~/.emacs.d/")
(setq plugins-dir (concat config-dir "plugins/"))

(add-to-list 'load-path plugins-dir)

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat plugins-dir "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)

(setq inhibit-startup-message t)
(setq scroll-step 1)

(tool-bar-mode nil)
(scroll-bar-mode nil)
(show-paren-mode t)

(global-unset-key (kbd "\C-z"))
(global-unset-key (kbd "\C-x \C-z"))

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
  (local-set-key (kbd "M-RET") 'lisp-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

(defun c/c++-mode-hook ()
  (c-set-style "linux")
  (gtags-mode t)
  (auto-complete-mode t)
  (local-set-key [(f5)] 'gud-gdb))
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat config-dir "yasnippets"))

(require 'gtags)
(global-set-key (kbd "\M-.") 'gtags-find-tag)
(global-set-key (kbd "\M-,") 'gtags-find-symbol)
(global-set-key (kbd "\C-x p") 'gtags-pop-stack)
