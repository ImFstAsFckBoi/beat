;;; beat.el --- Buffer Editing And Traversal  -*- lexical-binding:t -*-

;; Copyright (C) 2025  Martin Olivesten

;; Filename: beat.el
;; Author: Martin Olivesten <mbao02@pm.me>
;; URL: https://github.com/ImFstAsFckBoi/beat
;; Description: Utility functions for Buffer Editing And Traversal (BEAT)
;; Created: 2024-11-16
;; Updated: 2025-9-20
;; Package-Version: 1.0
;; Package-Requires: ((emacs "28.1") (compat "30") (multiple-cursors "1.5.0")

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
;; BEAT (Buffer Editing And Traversal) is a collection of utility functions to aid in traversing and editing buffers in a more familiar way.
;; This includes a series of functionalities ported from VS Code and other such CUA text editors.
;; Tries to, when possible, reuse standard elisp functions and use standard Emacs naming schemes (e.g. kill, save, dwim, etc.) for all functions.
;;
;; All functions are opt-in and you may pick and choose which commands you want
;; and which key-sequences you would like to bind them to.

;;; Installation:
;;
;; (require 'beat)


;;; Require
(require 'multiple-cursors)

;;; Code:

(defun beat-dwim-kill ()
  "Mimic VS Code functionality [C-x]: kill whole line if nothing is marked."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (kill-region (line-beginning-position) (+ (line-end-position) 1))))

(defun beat-dwim-save ()
  "Mimic VS Code functionality [C-c]: save whole line to kill-ring if nothing is marked."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (+ (line-end-position) 1))))


(defun beat-delete-mark-yank ()
  "PARTIALLY OBSOLETE! FUNCTIONALITY COVERED BY 'delete-selection-mode'.
Mimic VS Code functionality [C-v]: delete marked region when yanking."
  (interactive)
  (if (region-active-p)
      (delete-region (region-beginning) (region-end)))
  (yank))


(defun beat-mark-around-word ()
  "Mimic first part of VS Code functionality [C-d]: Mark the current word, forward and backwards."
  (interactive)
  ;; If at beginning of word, go to end first
  (if (not (eq (char-after) ? )) ; "? " = space
      (beat-right-to-boundary))
  (beat-left-to-boundary)
  (set-mark (point))
  (beat-right-to-boundary)
  (activate-mark))


(defun beat-mark-around-word-or-next-match ()
  "Mimic VS Code functionality [C-d]: If nothing is marked, mark word, if something is marked, mark next instance of it."
  (interactive)
  (if (region-active-p)
      (mc/mark-next-like-this (region-beginning))
    (beat-mark-around-word)))


(defun beat-duplicate-line-down ()
  "Mimic VS Code functionality [M-S-<down>]: Duplicate line down."
  (interactive)
  (duplicate-line)
  (next-line))


(defun beat-dwim-previous-line ()
  "Mimic VS Code behavior: Pressing up on the first line move the cursor to column 0."
  (interactive "^")
  (if (eq (line-number-at-pos) 1)
      (goto-char 0)
    (previous-line)))


(defun beat-dwim-move-beginning-of-line ()
  "Default behavior move to first non white-space character.
If at first non white-space character, move to beginning of line"
  (interactive "^")
  (let ((pos (point)))
    (back-to-indentation)
    (when (eq pos (point))
      (beginning-of-line))))


(setq-default beat--char-syntax-level-alist `((,(char-syntax ?a) . 0)
                                              (,(char-syntax ?-) . 0)
                                              (,(char-syntax ?/) . 0)
                                              (,(char-syntax ?\() . 100)
                                              (,(char-syntax ? ) . 200)
                                              (,(char-syntax ?\n) . 300)))


(defun beat--get-char-category (char)
  "Get the category of CHAR according to `beat-char-categories'."
  (or (cdr (assoc (char-syntax char)
                  beat--char-syntax-level-alist))
      999))

(defun beat--category-compare (f a b)
  "Compare the char-categories of A and B using F."
  (apply f (mapcar 'beat--get-char-category `(,a ,b))))

(defun beat--eval-or-funcall (f)
  "If F is a function, call it.  If F is an s-expression, evaluate it."
  (if (listp f)
      (eval f)
    (funcall f)))

(defun beat--apply-to-boundary (bexp aexp)
  "Evaluate BEXP before, and AEXP after, for every char up to a character boundary."
  ;; Set b and a to same so first while always passes
  (let ((b (char-before))
        (a (char-before))
        (count 0))

    (while (or (beat--category-compare 'eq a b)
               (and (eq count 1) (beat--category-compare '> b a)))
      (when bexp (beat--eval-or-funcall bexp))
      (setq b (char-before)
            a (char-after)
            count (+ count 1))
      (when aexp (beat--eval-or-funcall aexp)))))

(defun beat-delete-right-to-boundary ()
  "Mimic VS Code functionality [C-<delete>]: Deletes sequences forward, with more similar rules to VS Code."
  (interactive)
  (beat--apply-to-boundary 'right-char '(delete-char -1)))


(defun beat-delete-left-to-boundary ()
  "Mimic VS Code functionality [C-<backspace>]: Deletes sequences backwards, with more similar rules to VS Code."
  (interactive)
  (beat--apply-to-boundary 'left-char '(delete-char 1)))


(defun beat-right-to-boundary ()
  "Go forward for all characters in the same char category."
  (interactive "^")
  (beat--apply-to-boundary 'right-char nil))


(defun beat-left-to-boundary ()
  "Go backward for all characters in the same char category."
  (interactive "^")
  (beat--apply-to-boundary 'left-char nil))


;;; -- unused functions ---

(defun beat--relative-char-at (N)
  "Get the Nth char relative to point.  Negative N gives before point, positive after point.  N = 0 gives nil."
  (when (eq N 0)
    (if (< N 1)
        (char-before (+ (point) (+ N 1)))
      (char-after  (+ (point)  (- N 1))))))

(defun beat--eat-char (N)
  "Delete N chars an return the Nth char."
  (let ((c (beat--relative-char-at N)))
    (delete-char N)
    c))


(provide 'beat)
;;; beat.el ends here
