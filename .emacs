(setq config-dir "~/.emacs.d/")
(setq plugin-dir (concat config-dir "plugins/"))

(setq load-path (cons plugin-dir load-path))

(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat plugin-dir "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

(setq inhibit-startup-message t)
(setq scroll-step 1)

(tool-bar-mode nil)
(scroll-bar-mode nil)
(show-paren-mode t)

(global-unset-key (kbd "\C-z"))
(global-unset-key (kbd "\C-x \C-z"))

(case (window-system)
  ('x (progn
	(set-default-font "Monaco-8")))
  ('w32 (progn
	  (set-default-font "Monaco-8"))))

(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(mwheel-up)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mwheel-down)] '(lambda () (interactive) (scroll-up 3)))

(global-set-key [(f7)] 'compile)
(global-set-key [(f12)] 'eshell)

(defun elisp-mode-hook ()
  (local-set-key (kbd "M-RET") 'lisp-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

(defun c/c++-mode-hook ()
  (c-set-style "linux")
  (gtags-mode t)
  (auto-complete-mode t)
  (ecb-activate)
  (local-set-key [(f5)] 'gud-gdb))
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat plugin-dir "snippets"))

(require 'gtags)
(global-set-key (kbd "\M-.") 'gtags-find-tag)
(global-set-key (kbd "\M-,") 'gtags-find-symbol)
(global-set-key (kbd "\C-x p") 'gtags-pop-stack)

(require 'color-theme)
(defun color-theme-vardyh ()
  (interactive)
  (color-theme-install
   '(color-theme-vardyh
     ((background-color . "#051514")
      (background-mode . light)
      (border-color . "#000000")
      (cursor-color . "#fce94f")
      (foreground-color . "#b3b3b3")
      (mouse-color . "black"))
     (fringe ((t (:background "#000000"))))
     (mode-line ((t (:foreground "#b5b5b5" :background "#000000"))))
     (region ((t (:background "#104237"))))
     (font-lock-builtin-face ((t (:foreground "#da7cc7"))))
     (font-lock-comment-face ((t (:foreground "#424242"))))
     (font-lock-function-name-face ((t (:foreground "#e0a642"))))
     (font-lock-keyword-face ((t (:foreground "#5a83af"))))
     (font-lock-string-face ((t (:foreground "#bdb400"))))
     (font-lock-type-face ((t (:foreground"#6fbe23"))))
     (font-lock-variable-name-face ((t (:foreground "#d3543c"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" :bold t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     )))
(if (not (eq (window-system) nil))
    (progn
      (color-theme-vardyh)
      (mapc
       (lambda (face)
	 (set-face-attribute face nil :weight 'normal :underline nil))
       (face-list))))

(load-file (concat plugin-dir "cedet/common/cedet.el"))
(setq semanticdb-default-save-directory (concat config-dir "semanticdb"))
(semantic-load-enable-code-helpers)

(setq load-path (cons (concat plugin-dir "ecb") load-path))
(require 'ecb)
