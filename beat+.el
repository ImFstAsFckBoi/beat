;;; beat+.el --- Buffer Editing And Traveral Plus   -*- lexical-binding:t -*-

;; Filename: beat+.el
;; Description: Utility functions for Buffer Editing And Traveral (BEAT)
;; Author: Martin Olivesten <mbao02@pm.me>
;; Copyright (C) 2024  Martin Olivesten, all rights reserved.
;; Created: 2024-11-16

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; BEAT+ (Buffer Editing And Traversal Plus) is a collection of utility functions to aid in traversing and editing buffers in a more familiar way.
;; This includes a series of functionalities ported from VS Code and other such CUA text editors.
;; Tries to, when possible, reuse standard elisp functions and use standard Emacs naming schemes (e.g. kill, save, dwim, etc.) for all functions.
;;
;; All functions are opt-in and you may pick and choose which commands you want
;; and which key-sequences you would like to bind them to.

;;; Installation:
;;
;; (require 'beat+)


;;; Require
(require 'multiple-cursors)

;;; Code:

(defun beatp-dwim-kill ()
  "Mimic VS Code functionality [C-x]: kill whole line if nothing is marked."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
      (kill-region (line-beginning-position) (+ (line-end-position) 1))))

(defun beatp-dwim-save ()
  "Mimic VS Code functionality [C-c]: save whole line to kill-ring if nothing is marked."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
      (kill-ring-save (line-beginning-position) (+ (line-end-position) 1))))


(defun beatp-delete-selection-yank ()
  "PARTIALLY OBSOLETE! FUNCTIONALITY COVERED BY 'delete-selection-mode'.
Mimic VS Code functionality [C-v]: delete marked region when yanking."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end)))
  (yank))


(defun beatp-select-around-word ()
  "Mimic first part of VS Code functionality [C-d]: Mark the current word, forward and backwards"
  (interactive)
  (right-word)
  (left-word)
  (set-mark (point))
  (right-word)
  (activate-mark))

(defun beatp-select-around-word-or-next-match ()
  "Mimic VS Code functionality [C-d]: If nothing is marked, select word, if something is marked, select next instance of it."
  (interactive)
  (if (region-active-p)
    (mc/mark-next-like-this (region-beginning))
    (beatp-select-around-word)))


(defun beatp-duplicate-line-down ()
  "Mimic VS Code functionality [M-S-<down>]: Duplicate line down."
  (interactive)
  (duplicate-line)
  (next-line))


(defun beatp-dwim-previous-line ()
  "Mimic VS Code behaviour: Pressing up on the first line moves the cursor to column 0."
  (interactive)
  (if (eq (line-number-at-pos) 1)
      (goto-char 0)
      (previous-line)))


; TODO:
; - use native char categories
; - don't use byte-to-string conversions

(setq-default beatp-char-categories
              '((whitespace "[ \t]")
                (newline    "[\r\n]")
                (words      "[a-zA-Z0-9_\\-]")))

(defun beatp-char-at (N)
  (if (eq N 0) nil
    (if (< N 1)
        (char-before (+ (point) (+ N 1)))
        (char-after  (+ (point)  (- N 1))))))


(defun beatp-eat-char (N)
    (setq-local c (beatp-char-at N))
    (delete-char N)
    c)


(defun beatp-get-char-category (char)
  (catch 'r (dolist (cat beatp-char-categories)
              (when (string-match (nth 1 cat) char)
                (throw 'r (nth 0 cat))))))


(defun beatp-call-expression (f)
  (if (listp f)
      (apply (car f) (cdr f))
      (funcall f)))

(defun beatp-apply-to-boundary (bexp aexp)
  ;; Set b and a to same so first while always passes 
  (setq-local b (char-before) a (char-before))
  (while (eq (beatp-get-char-category (byte-to-string b))
             (beatp-get-char-category (byte-to-string a)))
    (if bexp (beatp-call-expression bexp))
    (setq-local b (char-before)
                a (char-after))
    (if aexp (beatp-call-expression aexp))))

(defun beatp-delete-right-to-boundary () 
  "Mimic VS Code functionality [C-<delete>]: Deletes sequences forward, with more similar rules to VS Code."
  (interactive)
  (beatp-apply-to-boundary 'right-char '(delete-char -1)))


(defun beatp-delete-left-to-boundary () 
  "Mimic VS Code functionality [C-<backspace>]: Deletes sequences backwards, with more similar rules to VS Code."
  (interactive)
  (beatp-apply-to-boundary 'left-char '(delete-char 1)))


(defun beatp-right-to-boundary (&optional ARG)
  "Goto forward for all characters in the same char category."
  (interactive "^p")
  (beatp-apply-to-boundary 'right-char nil))


(defun beatp-left-to-boundary (&optional ARG)
  "Goto backward for all characters in the same char category."
  (interactive "^p")
  (beatp-apply-to-boundary 'left-char nil))


;;; beat+.el ends here
