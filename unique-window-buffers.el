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
;; Last-Updated: Sun Sep 18 14:37:01 2011 (+0800)
;;           By: Le Wang
;;     Update #: 10
;; URL:
;; Keywords:
;; Compatibility:

;;; Installation:

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
;;
;; `bury-buffer' calls `switch-to-prev-buffer'
;;
;; `quit-window' calls `switch-to-prev-buffer'
;;
;; `kill-buffer' calls `replace-buffer-in-windows' to replace the buffer it
;; just killed, which calls `switch-to-prev-buffer'


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
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile (require 'cl))


(provide 'unique-window-buffers)


(defvar unique-window-buffers-uninteresting-filters
  '(minibufferp)
  "List of functions called in order with a buffer
  as the only parameter.  It should return t if the buffer is not
  interesting")

(defvar unique-window-buffers-mode nil
  "Set this variable to t if you want to only see unique buffers in different windows on the same frame.

Note: this does not have the baggage of a full minor-mode.  It's
just a variable, setting it has immediate effect")

(defun unique-window-buffers-show (&optional window)
  "Choose a buffer show in no other windows on the same frame to display in window.

If window is nil, then use the `selected-window'"
  (setq window (or window (selected-window)))
  (let ((other-displayed-buffers (delq nil
                                       (mapcar #'(lambda (w)
                                                   (when (not (eq w window))
                                                     (window-buffer w)))
                                               (window-list nil nil nil)))))
    (when (memq (current-buffer) other-displayed-buffers)
      (set-window-buffer
       window
       (or (some
            #'(lambda (b)
                (unless (or (memq b other-displayed-buffers)
                            (some #'(lambda (f) (funcall f b))
                                  unique-window-buffers-uninteresting-filters))
                  b))
            (buffer-list (selected-frame)))
           (current-buffer))))))

(defadvice switch-to-prev-buffer (around unique-window-buffers activate compile)
  (if unique-window-buffers-mode
      (let ((old-window (or (ad-get-arg 0)
                            (selected-window))))
        ad-do-it
        (unique-window-buffers-show old-window))
    ad-do-it))

(defadvice split-window (after unique-window-buffers activate compile)
  (when unique-window-buffers-mode
    (unique-window-buffers-show ad-return-value)))

;; if you really need to remove the advice: (ad-deactivate-regexp "\\`unique-window-buffers\\'")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unique-window-buffers.el ends here
