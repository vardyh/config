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
	  (set-default-font "Monaco-9"))))

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
  (color-theme-gnome2)
  (color-theme-install
   '(color-theme-vardyh
     ((background-color . "#002832")
      (background-mode . light)
      (border-color . "#000000")
      (cursor-color . "#ffffff")
      (foreground-color . "#d0d0d0")
      (mouse-color . "white"))
     (fringe ((t (:background "#000000"))))
     (mode-line ((t (:foreground "#b5b5b5" :background "#000000"))))
     (region ((t (:background "#707df5"))))
     (font-lock-builtin-face ((t (:foreground "#ffc0c0"))))
     (font-lock-comment-face ((t (:foreground "#808080"))))
     (font-lock-function-name-face ((t (:foreground "#ffe0c0"))))
     (font-lock-keyword-face ((t (:foreground "#bcdf57"))))
     (font-lock-string-face ((t (:foreground "#ffcd62"))))
     (font-lock-type-face ((t (:foreground"#c0c0ff"))))
     (font-lock-variable-name-face ((t (:foreground "#bcff79"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" t))))
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
(setq ecb-tip-of-the-day nil)

(setq ecb-stat nil)
(defun ecb-toggle-activate ()
  (interactive)
  (if ecb-stat
      (progn
	(ecb-deactivate)
	(setq ecb-stat nil))
    (progn
      (ecb-activate)
      (ecb-layout-switch "left9")
      (setq ecb-stat t))))
(global-set-key (kbd "\C-x <f9>") 'ecb-toggle-activate)

(custom-set-variables
 '(ecb-layout-window-sizes (quote (("left9" (0.18994413407821228 . 0.98)))))
 '(ecb-options-version "2.40"))

(require 'tabbar)
(tabbar-mode t)
(set-face-attribute 'tabbar-default nil
                    :family "Tahoma"
                    :background "gray80"
                    :foreground "gray30"
                    :height 1.0
                    )
(set-face-attribute 'tabbar-button nil
                    :inherit 'tabbar-default
                    :box '(:line-width 1 :color "gray30")
                    )
(set-face-attribute 'tabbar-unselected nil
                    :inherit 'tabbar-default
                    :box '(:line-width 2 :color "gray70")
                    )
