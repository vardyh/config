;; directories
(defconst config-dir "~/.emacs.d/")
(defconst plugin-dir (concat config-dir "plugins/"))
(setq load-path (cons plugin-dir load-path))

;; delete trailing whitespace before file saving
(add-hook 'before-save-hook (lambda () (delete-trailing-whitespace)))

;; common settings
(global-set-key (kbd "<f11>") 'eshell)
(global-set-key (kbd "\C-c \C-g") 'goto-line)
(global-unset-key (kbd "\C-z"))

(when (eq system-type 'windows-nt)
  (set-message-beep 'silent))
(when (eq system-type 'darwin)
  (setq aquamacs-scratch-file nil)
  (setq mac-command-modifier 'meta)
  (setq mac-command-key-is-meta t))

(setq inhibit-startup-message t)
(setq scroll-step 1)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(setq cursor-type 'box)
(setq default-cursor-type 'box)
(blink-cursor-mode 0)
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))
(set-variable 'tab-width 8)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs 'y-or-n-p)
(setq read-file-name-completion-ignore-case t)

(set-language-environment 'UTF-8)
(display-time)

(tool-bar-mode 0)
(scroll-bar-mode 0)
(show-paren-mode t)
(column-number-mode t)
(xterm-mouse-mode t)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-<f5>") 'revert-buffer)

;; smooth mouse scrolling
(global-set-key [(mouse-4)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mouse-5)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(wheel-up)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(wheel-down)] '(lambda () (interactive) (scroll-up 3)))
(global-set-key [(mwheel-up)] '(lambda () (interactive) (scroll-down 3)))
(global-set-key [(mwheel-down)] '(lambda () (interactive) (scroll-up 3)))

;; scrolling w/ keyboards
(global-set-key (kbd "C-M-p") '(lambda () (interactive) (scroll-down 3)))
(global-set-key (kbd "C-M-n") '(lambda () (interactive) (scroll-up 3)))

;; color theme
(require 'color-theme)
(defun color-theme-vardyh ()
  (interactive)
  (color-theme-gnome2)
  (color-theme-install
   '(color-theme-vardyh
     ((background-color . "#003050")
      (background-mode . light)
      (border-color . "#333333")
      (cursor-color . "#ffffff")
      (foreground-color . "#d0d0d0")
      (mouse-color . "white"))
     (fringe ((t (:background "#002832"))))
     (mode-line ((t (:foreground "#b5b5b5" :background "#333333"))))
     (region ((t (:background "#707da5"))))
     (font-lock-builtin-face ((t (:foreground "#f9d5d5"))))
     (font-lock-preprocessor-face ((t (:foreground "#e8c5c5"))))
     (font-lock-comment-face ((t (:foreground "#a0a0a0"))))
     (font-lock-doc-string-face ((t (:foreground "#c0a0a0"))))
     (font-lock-function-name-face ((t (:foreground "#ffe0c0"))))
     (font-lock-keyword-face ((t (:foreground "#e7bcbc"))))
     (font-lock-string-face ((t (:foreground "#fdf3a2"))))
     (font-lock-type-face ((t (:foreground"#9cbed8"))))
     (font-lock-variable-name-face ((t (:foreground "#d5f291"))))
     (minibuffer-prompt ((t (:foreground "#ffffff" t))))
     (font-lock-warning-face ((t (:foreground "Red" :bold t))))
     (isearch-lazy-highlight-face ((t (:background "#353535")))))))

(defun color-theme-solarized ()
  (interactive)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
  (add-to-list 'load-path "~/.emacs.d/themes")

  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  ;; make the modeline high contrast
  (setq solarized-high-contrast-mode-line t)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; Use more italics
  (setq solarized-use-more-italic t)

  ;; Use less colors for indicators such as git:gutter, flycheck and similar
  (setq solarized-emphasize-indicators nil)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)

  (load-theme 'solarized-light t))

(if (not (eq (window-system) nil))
    (progn
      ;; (color-theme-vardyh)
      (color-theme-solarized)
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
	  (set-default-font "ProFontWindows-10.5")
	  (set-fontset-font (frame-parameter nil 'font)
			    'han
			    '("Microsoft YaHei" . "unicode-bmp"))))
  ('ns (progn
	 (set-default-font "Monaco-10"))))

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

;; recent jump
(require 'recent-jump)
(global-set-key (kbd "C-M-,") 'recent-jump-jump-backward)
(global-set-key (kbd "C-M-.") 'recent-jump-jump-forward)

;; winner mode
(require 'winner)
(winner-mode t)
(global-set-key (kbd "C-c b") 'winner-undo)

;; auto complete
(require 'auto-complete)
(add-to-list 'ac-dictionary-directories (expand-file-name "~/.ac-dict"))
(require 'auto-complete-config)
(eval-after-load "auto-complete" '(add-to-list 'ac-modes 'lua-mode))
;; (ac-config-default)
;; (global-auto-complete-mode t)
;; (setq ac-auto-start t)

;; (setq ac-sources '(ac-source-semantic
;; 		   ac-source-yasnippet
;; 		   ac-source-abbrev
;; 		   ac-source-words-in-buffer
;; 		   ac-source-words-in-all-buffer
;; 		   ac-source-imenu
;; 		   ac-source-files-in-current-dir
;; 		   ac-source-filename))

(defun ac-complete-semantic-self-insert (arg)
  (interactive "p")
  (self-insert-command arg)
  (ac-complete-semantic))

;; session
(load-file (concat plugin-dir "session.el"))
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
(add-to-list 'session-globals-exclude
             'org-mark-ring)

;; hexview-mode
(require 'hexview-mode)
(global-set-key (kbd "\C-h \C-f") 'hexview-find-file)

;; unicad
(require 'unicad)
(setq file-name-coding-system 'utf-8)

;; dired-mode hooks
(defun dired-hooks ()
  (local-set-key (kbd "!") 'dired-do-async-shell-command))
(add-hook 'dired-mode-hook 'dired-hooks)

;; shell mode support ansi colors
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;
; programming related configurations
; ==================================
;

;; gud-gdb keybinding
(global-set-key (kbd "C-x g") 'gud-gdb)

;; sln-mode
(require 'sln-mode)
(autoload 'find-sln "sln-mode")

;; auto mode
(setq auto-mode-alist
      (cons '("\\.S" . asm-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.sig" . lua-mode) auto-mode-alist))

;; cedet
(setq semanticdb-default-save-directory (expand-file-name "~/.semanticdb"))
(global-ede-mode t)

(global-set-key (kbd "C-M-g") 'semantic-ia-fast-jump)
(global-set-key (kbd "C-x p")
		(lambda()
		  (interactive)
		  (if (ring-empty-p (oref semantic-mru-bookmark-ring ring))
		      (error "Semantic Bookmark ring is currently empty"))
		  (let* ((ring (oref semantic-mru-bookmark-ring ring))
			 (alist (semantic-mrub-ring-to-assoc-list ring))
			 (first (cdr (car alist))))
		    (if (semantic-equivalent-tag-p (oref first tag)
						   (semantic-current-tag))
			(setq first (cdr (car (cdr alist)))))
		    (semantic-mrub-switch-tags first))))

;; ecb
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

;; gtags mode
(require 'gtags)
(global-set-key (kbd "\C-c \C-f") 'gtags-find-with-grep)
(global-set-key (kbd "\C-c \C-r") 'gtags-find-rtag)
(global-set-key (kbd "<f12>") '(lambda ()
				 (interactive)
				 (shell-command "global -u")))

;; c/c++ mode
(defun c/c++-mode-hook ()
  (c-set-style "linux")
  (auto-complete-mode t)
  (hs-minor-mode t)
  (gtags-mode t)
  (toggle-truncate-lines t)
  (local-set-key (kbd "M-\\") 'hs-toggle-hiding)
  (local-set-key (kbd "C-M-/") 'semantic-ia-complete-symbol-menu)
;  (local-set-key "." 'ac-complete-semantic-self-insert)
;  (local-set-key ">" 'ac-complete-semantic-self-insert)

  (let ((filename (buffer-file-name)))
    (when (or
	   (string-match "/works/unicorn" filename)
	   (string-match "/vxeng-dt/" filename))
      (setq indent-tabs-mode nil)
      (setq c-basic-offset 4)
      (setq tab-width 4))
    )
  )
(add-hook 'c-mode-hook 'c/c++-mode-hook)
(add-hook 'c++-mode-hook 'c/c++-mode-hook)

;; (defmacro define-new-c-style (name derived-from style-alist match-path)
;;   `(progn
;;      (c-add-style ,name
;;                   '(,derived-from ,@style-alist))
;;      (add-hook 'c-mode-hook
;;                (lambda ()
;;                  (let ((filename (buffer-file-name)))
;;                    (when (and filename
;;                               (string-match match-path filename))
;;                      (c-set-style ,name)))))))
;; (define-new-c-style "unicorn" "gnu" ((setq tab-width 4)
;; 				     (setq indent-tabs-mode nil))
;;   "/src/unicorn")

;; assembly mode
(defun assembly-mode-hook ()
  (gtags-mode t))
(add-hook 'asm-mode-hook 'assembly-mode-hook)

;; cdb/kd mode
(when (eq system-type 'windows-nt)
  (load-file (concat plugin-dir "cdb-gud.el"))
  (global-set-key (kbd "<f5>") 'gud-cont)
  (global-set-key (kbd "<f9>") 'gud-break)
  (global-set-key (kbd "<f10>") 'gud-next)
  (global-set-key (kbd "<f11>") 'gud-step)
  (global-set-key (kbd "C-<f11>") 'gud-finish)
  (global-set-key (kbd "C-<f5>") 'gud-tbreak))

;; yasnippet
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat plugin-dir "snippets"))
(global-set-key (kbd "M-/") 'yas/expand)

;; git support
(require 'magit)

;; nsis mode
(require 'nsis-mode)

;; smart compile
(defun smart-compile ()
  (interactive)
  (if (not (null (file-exists-p "Project.ede")))
      (ede-compile-project)
    (cond ((eq system-type 'windows-nt) (compile-winnt))
	(t (compile-gnu)))))

(defun compile-winnt ()
  (let ((origin-defdir default-directory)
	(sln-path
	 (loop
	  for d = default-directory then (expand-file-name ".." d)
	  if (car (directory-files d nil "\\.sln$")) return d
	  if (string-match "^[a-zA-Z]:/$" d) return nil)))
    (if (not (null sln-path))
	(progn
	  (setq default-directory sln-path)
	  (compile-w/-command (format "devenv.com \"%s\" /build Debug"
				      (expand-file-name
				       (car (directory-files sln-path nil "\\.sln$"))
				       sln-path)))))
    (setq default-directory origin-defdir)))

(defun compile-gnu ()
  (let ((origin-defdir default-directory)
	(makefile-path
	 (loop
	  for d = default-directory then (expand-file-name ".." d)
	  if (file-exists-p (expand-file-name "Makefile" d)) return d
	  if (equal d (expand-file-name "/")) return nil)))
    (if (not (null makefile-path))
	(progn
	  (setq default-directory makefile-path)
	  (compile-w/-command (format "cd %s; make " makefile-path))))
    (setq default-directory origin-defdir)))

(defun compile-w/-command (command)
  (if (not (null command))
      (let ((command (read-from-minibuffer "Compile command: " command)))
	(compile command))))

(defun devenv ()
  (interactive)
  (let ((origin-defdir default-directory)
	(sln-path
	 (loop
	  for d = default-directory then (expand-file-name ".." d)
	  if (car (directory-files d nil "\\.sln$")) return d
	  if (string-match "^[a-zA-Z]:/$" d) return nil)))
    (if (not (null sln-path))
	(progn
	  (setq default-directory sln-path)
	  (compile-w/-command (format "devenv.com %s /build Debug"
				      (expand-file-name
				       (car (directory-files sln-path nil "\\.sln$"))
				       sln-path)))))
    (setq default-directory origin-defdir)))

(global-set-key (kbd "C-x <f7>") 'smart-compile)
(global-set-key (kbd "<f7>") 'devenv)

;; lua mode
(require 'lua-mode)

;; go mode
(add-to-list 'load-path (concat plugin-dir "go-mode"))
(require 'go-mode-autoloads)

;; nsis mode
(autoload 'nsis-mode "nsis-mode" "NSIS mode" t)
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Ii]\\)$" .
                                 nsis-mode)) auto-mode-alist))
(setq auto-mode-alist (append '(("\\.\\([Nn][Ss][Hh]\\)$" .
                                 nsis-mode)) auto-mode-alist))

;; company-mode
(add-to-list 'load-path (concat plugin-dir "company-0.5"))
(autoload 'company-mode "company" nil t)

;; use gtags-mode in dired-mode
(add-hook 'dired-mode-hook '(lambda () (gtags-mode t)))

;; eassist
(require 'eassist)
(global-set-key (kbd "C-c h") 'eassist-switch-h-cpp)

;; markdown
(require 'markdown-mode)

;;
; life style
; ==========
;

;; emms
(add-to-list 'load-path (concat plugin-dir "emms-3.0"))
(require 'emms)
(require 'emms-setup)
(require 'emms-volume)
(emms-standard)
(emms-default-players)

;; emms global keybindings
(global-set-key (kbd "C-c e g") 'emms-playlist-mode-go)
(global-set-key (kbd "C-c e t") 'emms-play-directory-tree)
(global-set-key (kbd "C-c e x") 'emms-start)
(global-set-key (kbd "C-c e v") 'emms-stop)
(global-set-key (kbd "C-c e n") 'emms-next)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e o") 'emms-show)
(global-set-key (kbd "C-c e h") 'emms-shuffle)
(global-set-key (kbd "C-c e e") 'emms-play-file)
(global-set-key (kbd "C-c e f") 'emms-play-playlist)
(global-set-key (kbd "C-c e SPC") 'emms-pause)
(global-set-key (kbd "C-c e a") 'emms-add-directory-tree)

;; emms playlist-mode keybindings
(define-key emms-playlist-mode-map (kbd "SPC") 'emms-pause)
(define-key emms-playlist-mode-map (kbd "+") 'emms-volume-raise)
(define-key emms-playlist-mode-map (kbd "-") 'emms-volume-lower)
(define-key emms-playlist-mode-map (kbd "<right>")
  (lambda () (interactive) (emms-seek +10)))
(define-key emms-playlist-mode-map (kbd "<left>")
  (lambda () (interactive) (emms-seek -10)))
(define-key emms-playlist-mode-map (kbd "<up>")
  (lambda () (interactive) (emms-seek +60)))
(define-key emms-playlist-mode-map (kbd "<down>")
  (lambda () (interactive) (emms-seek -60)))

;; start server
(server-start)
(put 'upcase-region 'disabled nil)
