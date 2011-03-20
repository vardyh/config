;;; recent-jump.el --- jump back to where you start a big jump

;; Copyright (C) 2005  Free Software Foundation, Inc.

;; Author: ChunYe Wang ;;Keywords: tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; sometimes, we start a big jump, e.g. go to the beginning of buffer, search,
;; page down etc, it is handy that you can go back to where you start. For
;; example, when we writing program source code, we ofter search for some
;; reference and go back to where you start and continue writing. But how can we
;; define "Big Jump". I can not find a proper hook for the big jump, so at last
;; I choose the pre-command-hook. Now what my defination for "Big Jump" is :
;; 
;; 1. you issued some special command, it can be defined by
;; recent-jump-hook-commands 
;;
;; 2. you jumped really much. that is you move more than `recent-jump-threshold'
;; lines.
;;
;; C-o is binded for open-lines, I not used to very often, and VI bind C-o for
;; go to old point, so it is a good idea to bind "C-o" for jumping back where
;; you start. and "M-o" is suitable for jump forward, i.e. undo the jumping back.
;;
;; sample configuration
;; (setq recent-jump-threshold 4)
;; (setq recent-jump-ring-length 10)
;; (global-set-key (kbd "C-o") 'recent-jump-jump-backward)
;; (global-set-key (kbd "M-o") 'recent-jump-jump-forward)
;; (require 'recent-jump)

;;; Code:

(defvar recent-jump-threshold 5
  "how to define a big jump.")
(defvar recent-jump-ring-length 10)
(defvar recent-jump-ring (make-ring recent-jump-ring-length))

(defvar recent-jump-hook-commands
  '(next-line 
    previous-line
    isearch-forward
    isearch-backward
    end-of-buffer
    beginning-of-buffer
    pager-page-down
    pager-page-up
    beginning-of-defun
    end-of-defun
    forward-word
    backward-word
    forward-sexp
    backward-sexp
    scroll-up
    scroll-down
    find-tag
    mark-whole-buffer
    switch-to-buffer
    ido-switch-buffer
    ))
    
;;this variable is set at pre-command-hook, and remember where are you before a
;;command, and after the command executed, check this variable, weather it is a
;;big jump, if so, remember where you start the jump in the recent-jump-ring.
(setq recent-jump-where-are-you nil)
(defun recent-jump-pre-command()
  (if (memq this-command recent-jump-hook-commands)
      (progn (setq recent-jump-where-are-you (make-marker))
             (set-marker recent-jump-where-are-you (point)))
    (if (or (active-minibuffer-window)
            isearch-mode)
        nil; 
      (setq recent-jump-where-are-you nil))))
(defun recent-jump-insert-point()
  (if (and (not (ring-empty-p recent-jump-ring))
           (equal recent-jump-where-are-you (ring-ref recent-jump-ring 0)))
      nil ; don't insert the point if it is already exists.
    (ring-insert recent-jump-ring recent-jump-where-are-you)))

(defun recent-jump-pre-command()
  (if (and (memq this-command recent-jump-hook-commands) 
           (not (active-minibuffer-window))
           (not isearch-mode))
      (progn (setq recent-jump-where-are-you (make-marker))
             (set-marker recent-jump-where-are-you (point)))
    (setq recent-jump-where-are-you nil)))
(defun recent-jump-post-command()
  (when recent-jump-where-are-you
    ;; only remember jump in a the same buffer.
    (let* ((distance (if (eq (marker-buffer recent-jump-where-are-you)
                             (current-buffer))
                         (count-lines (point) recent-jump-where-are-you)
                       (1+ recent-jump-threshold))))
      (if (> distance recent-jump-threshold) 
          (recent-jump-insert-point)))))

(add-hook 'pre-command-hook 'recent-jump-pre-command)
(add-hook 'post-command-hook 'recent-jump-post-command)

;;(remove-hook 'pre-command-hook 'recent-jump-pre-command)
;;(remove-hook 'post-command-hook 'recent-jump-post-command)

(defun recent-jump-jump-backward (arg)
  (interactive "*p")
  (setq recent-jump-back-internal-counter 
        (if (eq last-command 'recent-jump-jump-backward)
            (+ recent-jump-back-internal-counter arg)
           (setq recent-jump-where-are-you (make-marker))
          (set-marker recent-jump-where-are-you (point))
          (recent-jump-insert-point)
          1))
  (if (ring-empty-p recent-jump-ring)
      (error "jump ring is empty.")
    (let ((m (ring-ref recent-jump-ring recent-jump-back-internal-counter)))
      (switch-to-buffer (marker-buffer m))
      (goto-char m))))
(defun recent-jump-jump-forward (arg)
  (interactive "*p")
  (setq this-command 'recent-jump-jump-backward)
  (recent-jump-jump-backward(* -1 arg)))

(provide 'recent-jump)