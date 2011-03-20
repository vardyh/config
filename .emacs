;; directories
(setq config-dir "~/.emacs.d/")
(setq plugin-dir (concat config-dir "plugins/"))
(setq load-path (cons plugin-dir load-path))

;; common settings
(setq inhibit-startup-message t)
(setq scroll-step 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)
(display-battery-mode)

(tool-bar-mode nil)
(scroll-bar-mode nil)
(show-paren-mode t)
(column-number-mode t)

;; ido mode
(require 'ido)
(ido-mode t)

;; recent jump
(require 'recent-jump)
(global-set-key (kbd "C-c n") 'recent-jump-jump-forward)
(global-set-key (kbd "C-c p") 'recent-jump-jump-backward)

;; auto complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (concat plugin-dir "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)
(global-auto-complete-mode t)

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat plugin-dir "snippets"))
(global-set-key (kbd "M-/") 'yas/expand)

;; gtags
(require 'gtags)
(gtags-mode t)
(defun gtags-find-current-word ()
  (interactive)
  (let (tagname prompt input)
    (setq tagname (current-word))
    (if tagname
      (setq prompt (concat "Find tag: (default " tagname ") "))
     (setq prompt "Find tag: "))
    (setq input (completing-read prompt 'gtags-completing-gtags
                  nil nil nil gtags-history-list))
    (if (not (equal "" input))
      (setq tagname input))
    (gtags-push-context)
    (gtags-goto-tag tagname "")))
(global-set-key (kbd "M-.") 'gtags-find-current-word)
(global-set-key (kbd "M-,") 'gtags-find-symbol)
(global-set-key (kbd "C-x p") 'gtags-pop-stack)

;; cedet
(load-file (concat plugin-dir "cedet/common/cedet.el"))
(setq semanticdb-default-save-directory (concat config-dir "semanticdb"))
(semantic-load-enable-code-helpers)
(global-ede-mode t)

;; ecb
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
(custom-set-variables
 '(ecb-layout-window-sizes (quote (("left9" (0.19 . 0.98)))))
 '(ecb-options-version "2.40"))
(global-set-key (kbd "\C-x <f9>") 'ecb-toggle-activate)

;; tabbar
(require 'tabbar)
(tabbar-mode t)
(set-face-attribute
 'tabbar-default nil
 :family "Tahoma"
 :background "gray80"
 :foreground "gray30"
 :height 1.0)
(set-face-attribute
 'tabbar-button nil
 :inherit 'tabbar-default
 :box '(:line-width 1 :color "gray30"))
(set-face-attribute
 'tabbar-unselected nil
 :inherit 'tabbar-default
 :box '(:line-width 2 :color "gray70"))

;; line number
(require 'linum)
(global-linum-mode t)

;; layout
(require 'layout-restore)
(global-set-key (kbd "C-c l") 'layout-save-current)
(global-set-key (kbd "C-c d") 'layout-delete-current)

;; emacs lisp mode
(defun elisp-mode-hook ()
  (local-set-key (kbd "M-RET") 'lisp-complete-symbol))
(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hook)

;; c/c++ mode
(defun c/c++-mode-hook ()
  (c-set-style "linux")
  (c-toggle-hungry-state t)
  (gtags-mode t)
  (auto-complete-mode t)
  (hs-minor-mode t)
  (local-set-key (kbd "M-\\") 'hs-toggle-hiding)
  (local-set-key (kbd "M-RET") 'ac-complete-semantic)
  (local-set-key [(f5)] 'gud-gdb))
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

;; gdb mode
(defun gud/gdb-mode-hook ()
  (local-set-key [(f10)] 'gud-next)
  (local-set-key [(f11)] 'gud-step))
(add-hook 'gud-gdb-mode-hook 'gud/gdb-mode-hook)

;; unset exit keybindings
(global-unset-key (kbd "\C-z"))
(global-unset-key (kbd "\C-x \C-z"))

;; utilities
(global-set-key [(f3)] 'dired)
(global-set-key [(f5)] 'ede-debug-target)
(global-set-key [(shift f5)] 'ede-run-target)
(global-set-key [(f7)] 'ede-compile-target)
(global-set-key [(f12)] 'eshell)

;; smooth mouse scrolling
(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(mwheel-up)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mwheel-down)] '(lambda () (interactive) (scroll-up 3)))

;; search current word
(defun isearch-current-word (func)
  (let ((cur-word (current-word)))
    (if (not cur-word)
	(message "(current-word) == nil")
      (call-interactively func)
      (isearch-yank-string cur-word))))
(defun isearch-current-word-forward (&optional backward)
  (interactive "P")
  (isearch-current-word 'isearch-forward))
(global-set-key (kbd "C-#") 'isearch-current-word-forward)

;; color theme
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
     (fringe ((t (:background "#002832"))))
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
     (isearch-lazy-highlight-face ((t (:background "#353535")))))))
(if (not (eq (window-system) nil))
    (progn
      (color-theme-vardyh)
      (mapc (lambda (face)
	      (set-face-attribute face nil :weight 'normal :underline nil))
	    (face-list))))

;; font setting
(case (window-system)
  ('x (progn
	(set-default-font "Monaco-8")
	(set-fontset-font (frame-parameter nil 'font)
			  'han
			  '("Microsoft YaHei" . "unicode-bmp"))))
  ('w32 (progn
	  (set-default-font "Monaco-9")
	  (set-fontset-font (frame-parameter nil 'font)
			    'han
			    '("Microsoft YaHei" . "unicode-bmp")))))
