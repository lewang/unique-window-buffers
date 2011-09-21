;;; unique-window-buffers.el --- try to avoid showing same buffer in two windows on the same frame

;; this file is not part of Emacs

;; Copyright (C) 2011 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: try to avoid showing same buffer in two windows on the same frame
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Sep 17 20:44:06 2011 (+0800)
;; Version: 0.1
;; Last-Updated: Wed Sep 21 11:14:11 2011 (+0800)
;;           By: Le Wang
;;     Update #: 29
;; URL: https://github.com/lewang/unique-window-buffers
;; Keywords:
;; Compatibility:

;;; Installation:

                                    ;;;;;;;;;;;;;;;;;;;;;;;;
                                    ;;                    ;;
                                    ;;                    ;;
                                    ;;    this is BETA    ;;
                                    ;;                    ;;
                                    ;;                    ;;
                                    ;;;;;;;;;;;;;;;;;;;;;;;;


;; add to .emacs.el:
;;
;;     (require 'unique-window-buffers)
;;     (setq unique-window-buffers-mode t)
;;

;;; Commentary:

;; I was annoyed that when splitting windows the same buffer shows up in the
;; new window.  It also happens when quitting help.  So I made this.
;;
;;

;; Some notes for the author:
;;
;; `split-window-*' all call `split-window'
;;
;; `bury-buffer' calls `switch-to-prev-buffer'
;;
;; `quit-window' calls `switch-to-prev-buffer', but also has a separate
;;   code-path that restores directly from window parameter `quit-restore'
;;
;; `kill-buffer' first kills the buffer, then calls
;; `replace-buffer-in-windows' to scrub the buffer from other windows.
;; `kill-buffer' has to be adviced to make the replace buffer unique.
;;
;;
;; `replace-buffer-in-windows' calls `switch-to-prev-buffer'
;;
;;
;; So advice functions `switch-to-prev-buffer' `quit-window' `split-window'
;; `kill-buffer'


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.a
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))


(provide 'unique-window-buffers)


(defvar unique-window-buffers-uninteresting-filters
  (eval-when-compile
    (list
     'minibufferp
     "\\` "
     (concat "\\`"
             (regexp-quote "*Pp Eval Output*")
             "\\'")
     (concat "\\`"
             (regexp-quote "*Completions*")
             "\\'")))
  "List of regexps matched against buffer-name or functions
  called with a buffer as the only parameter.  It should return t
  if the buffer is not interesting")

(defvar unique-window-buffers-mode nil
  "Set this variable to t if you want to only see unique buffers in different windows on the same frame.

Note: this does not have the baggage of a full minor-mode.  It's
just a variable, setting it has immediate effect")

(defun unique-window-buffers-show (&optional window filters)
  "Choose a buffer show in no other windows on the same frame to display in window.

If WINDOW is nil, then use the `selected-window'

FILTER is a list similar to `unique-window-buffers-uninteresting-filters'"
  (setq window (or window (selected-window)))
  (setq filters (or filters unique-window-buffers-uninteresting-filters))
  (let ((other-displayed-buffers (delq nil
                                       (mapcar #'(lambda (w)
                                                   (when (not (eq w window))
                                                     (window-buffer w)))
                                               (window-list nil nil nil)))))
    (when (memq (window-buffer window) other-displayed-buffers)
      (set-window-buffer
       window
       (or (dolist (b (buffer-list (selected-frame)))
             (unless (or (memq b other-displayed-buffers)
                         (dolist (f filters)
                           (let ((res (cond ((stringp f)
                                             (string-match f (buffer-name b)))
                                            ((functionp f)
                                             (funcall f b))
                                            (t
                                             (signal 'invalid-argument-error (list f))))))
                             (when res
                               (return res)))))
               (return b)))
           (current-buffer))))))

(defadvice switch-to-prev-buffer (around unique-window-buffers activate compile)
  (if unique-window-buffers-mode
      (let* ((old-window (or (ad-get-arg 0)
                             (selected-window)))
             (old-buffer (window-buffer old-window)))
        ad-do-it
        (when (window-live-p old-window)
          (unique-window-buffers-show
           old-window
           (nconc (if (buffer-live-p old-buffer)
                      ;; if the old buffer wasn't killed or buried, we need to
                      ;; prevent it from being switched to
                      (list (regexp-quote (concat "\\`"
                                                  (buffer-name old-buffer)
                                                  "\\'")))
                    nil)
                  unique-window-buffers-uninteresting-filters))))
    ad-do-it))

(defadvice quit-window (around unique-window-buffers activate compile)
  (if unique-window-buffers-mode
      (let (
            ;; `switch-to-prev-buffer' possibly called, mayhem ensues if we
            ;; let them do work
            (unique-window-buffers-mode nil)
            (old-window (or (ad-get-arg 1)
                            (selected-window))))
        ad-do-it
        (when (window-live-p old-window)
          (unique-window-buffers-show old-window)))
    ad-do-it))

(defadvice kill-buffer (around unique-window-buffers activate compile)
  (if unique-window-buffers-mode
      (let* ((my-buf (window-normalize-buffer (ad-get-arg 0)))
             (old-window (when (eq (window-buffer) my-buf)
                           (selected-window))))
        ad-do-it
        (when (window-live-p old-window)
          (unique-window-buffers-show old-window)))
    ad-do-it))

(defadvice split-window (after unique-window-buffers activate compile)
  (when unique-window-buffers-mode
    (unique-window-buffers-show ad-return-value)))

;; if you really need to remove the advice: (ad-deactivate-regexp "\\`unique-window-buffers\\'")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unique-window-buffers.el ends here
