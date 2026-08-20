;;; typewriter.el --- Turn Emacs into a text adder  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Enrico Flor

;; Author: Enrico Flor <enrico@eflor.net>
;; Maintainer: Enrico Flor <enrico@eflor.net>
;; URL: https://github.com/enricoflor/typewriter-mode
;; Version: 0.1.0
;; Keywords: convenience

;; Package-Requires: ((emacs "30.1"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides a small major mode called typewriter-mode.
;; Based on fundamental mode, it deliberately handicaps Emacs to an
;; extreme degree in order to provide something as close as possible
;; to the strict forward-only typewriter experience.  Some find that
;; the lack of editing facilities fosters a state of concentration and
;; focus that makes certain types of creative writing more satisfying.
;;
;; The package has seven configuration options:
;;
;;   - typewriter-silent: If non-nil, suppress all typewriter bell
;;     sounds.
;;
;;   - typewriter-preserve-undo-history: If t, you'll be able to undo
;;     edits if you intentionally leave typewriter-mode.  That action
;;     in itself requires confirmation and activates text-mode.
;;
;;   - typewriter-fill-column: If nil, your lines will be as long as
;;     you want.  Set to any integer N, emacs-turned-to-typewriter
;;     refuses to type anything once you reach column N until you
;;     press RET to open a new line.
;;
;;   - typewriter-warning-bell-offset: If your fill column is set,
;;     a physical 'ding' will sound this many characters before the
;;     margin to warn you that the carriage is about to jam.
;;
;;   - typewriter-show-chars-remaining: If this and
;;     typewriter-fill-column are non-nil, the modeline will display
;;     how many characters are left in the current line until the fill
;;     column (the margin) is reached.
;;
;;   - typewriter-modeline-format: Format string for the modeline
;;     indicator of remaining characters.  Default is " [%d]".
;;
;;   - typewriter-tab-width: Self explanatory.
;;
;; There are two hooks available for the user to customize the typing
;; experience, no function is added to them by default:
;;
;;   - typewriter-keystroke-hook
;;
;;   - typewriter-carriage-return-hook
;;
;; Although no systematic test has been carried out, this package's
;; minimality should ensure its compatibility with packages that
;; change the layout of text in the window, such as the fairly popular
;; olivetti, or any configuration that (for instance) hides or alters
;; element of the emacs interface.

;;; Code:

(defgroup typewriter nil
  "Configuration options for `typewriter-mode'."
  :prefix "typewriter-"
  :link '(url-link :tag "Website for typewriter-mode"
                   "https://github.com/enricoflor/typewriter-mode")
  :group 'convenience)

(defcustom typewriter-silent nil
  "If non-nil, suppress all typewriter bell sounds."
  :type 'boolean
  :group 'typewriter)

(defcustom typewriter-preserve-undo-history t
  "If non-nil, keep tracking undo history while in `typewriter-mode'.

You still cannot undo while the mode is active (buffer will be in
`read-only-mode'), but the history will be fully available after you
exit the mode."
  :type 'boolean
  :group 'typewriter)

(defcustom typewriter-fill-column nil
  "The margin limit in `typewriter-mode'.

If set to an integer, the typewriter will lock up and \\='ding\\=' when
you reach this column. You must press RET to continue.  If nil, no
margin is enforced."
  :type '(choice (const :tag "No margin" nil)
                 (integer :tag "Margin at column"))
  :group 'typewriter)

(defcustom typewriter-warning-bell-offset 8
  "Number of columns before the margin to sound the warning bell."
  :type '(choice (const :tag "No warning bell" nil)
                 (integer :tag "Warning bell offset at"))
  :group 'typewriter)

(defcustom typewriter-show-chars-remaining t
  "If non-nil, show columns remaining before the margin in the mode line.

Has no effect unless `typewriter-fill-column' is also set to an integer,
since without a margin there is nothing to count down to."
  :type 'boolean
  :group 'typewriter)

(defcustom typewriter-modeline-format " [%d]"
  "Format string for the modeline character counter.

This is passed directly to `format'.  The `%d' construct will be
replaced by the number of remaining characters.  Include a leading space
to visually separate the counter from preceding items in the modeline."
  :type 'string
  :group 'typewriter)

(defcustom typewriter-tab-width 8
  "The number of columns a tab key advances the carriage."
  :type 'integer
  :group 'typewriter)

(defcustom typewriter-keystroke-hook nil
  "Hook run after successfully striking a key (inserting a character)."
  :type 'hook
  :group 'typewriter)

(defcustom typewriter-carriage-return-hook nil
  "Hook run after inserting a new line."
  :type 'hook
  :group 'typewriter)

(defun typewriter--ding ()
  "Force a raw audio beep."
  (unless typewriter-silent
    (let ((ring-bell-function nil)
          (visible-bell nil))
      (ding))))

(defun typewriter--modeline-remaining ()
  "Return a modeline string with columns left before the margin.

Returns the empty string outside `typewriter-mode', or when
`typewriter-show-chars-remaining' or `typewriter-fill-column' is nil."
  (if (and (derived-mode-p 'typewriter-mode)
           typewriter-show-chars-remaining
           typewriter-fill-column)
      (let ((curr (save-excursion (end-of-line)
                                  (current-column))))
        (format typewriter-modeline-format
                (max 0 (- typewriter-fill-column curr))))
    ""))

(defun typewriter-backward-char ()
  "Move the carriage left without deleting, allowing overstrikes."
  (interactive)
  (let ((active-line-start (save-excursion
                             (goto-char (point-max))
                             (line-beginning-position))))
    (if (> (point) active-line-start)
        (backward-char 1)
      ;; carriage can't go back further than the left margin!
      (typewriter--ding)
      (let ((message-log-max nil))
        (message "Carriage is at the left margin!")))))

(defun typewriter-tab ()
  "Glide the carriage to the next tab stop without erasing existing ink."
  (interactive)
  (let* ((col (current-column))
         ;; calculate the next multiple of the tab width
         (next-stop (* (/ (+ col typewriter-tab-width) typewriter-tab-width)
                       typewriter-tab-width)))
    (let ((inhibit-read-only t))
      ;; move-to-column with 't' automatically pads spaces only if needed
      (move-to-column next-stop t))))

(defun typewriter--bell-ring (&optional maybe)
  "Ring the typewriter bell, or ring it only near the margin.

If MAYBE is nil, ring unconditionally.  If MAYBE is non-nil, ring only
when point is `typewriter-warning-bell-offset' columns short of
`typewriter-fill-column' and the current command is not `newline'."
  (when (or (not maybe)
            (and typewriter-fill-column
                 typewriter-warning-bell-offset
                 (= (current-column)
                    (- typewriter-fill-column
                       typewriter-warning-bell-offset))
                 (not (eq this-command 'newline))))
    (typewriter--ding)))

(defun typewriter--pre-command ()
  "Prepare buffer for typing, enforcing margins and ink permanence."
  (when (memq this-command '(self-insert-command
                             newline
                             typewriter-tab
                             typewriter-backward-char))

    (when (and (not (eq this-command 'typewriter-backward-char))
               (or (eq this-command 'newline)
                   (< (point) (save-excursion
                                (goto-char (point-max))
                                (line-beginning-position)))))
      (goto-char (point-max)))

    (let* ((col (current-column)))
      (cond
       ((and (eq this-command 'self-insert-command)
             (not (eobp))
             (not (looking-at-p "\t\\|\s")))
        ;; trying to type over existing ink
        (typewriter--bell-ring)
        (let ((message-log-max nil))
          (message "You can only overstrike blank spaces.  M-x typewriter-quit to get out to text-mode"))
        (setq this-command 'ignore))

       ((and typewriter-fill-column
             (not (eq this-command 'newline))
             (>= col typewriter-fill-column))
        ;; we're at the margin
        (typewriter--bell-ring t)
        (typewriter--bell-ring)
        (let ((message-log-max nil))
          (message "Margin reached! Press RET to return the carriage."))
        (setq this-command 'ignore))

       (t
        ;; just type
        (typewriter--bell-ring t)
        (let ((inhibit-read-only t))
          (when (and (eq this-command 'self-insert-command)
                     (looking-at-p "\t\\|\s"))
            (delete-char 1)))
        (setq-local inhibit-read-only t))))))

(defun typewriter--post-command ()
  "Restore the read-only shield after the keystroke, run hooks."
  (when (local-variable-p 'inhibit-read-only)
    (kill-local-variable 'inhibit-read-only))
  (cond ((eq this-command 'self-insert-command)
         (run-hooks 'typewriter-keystroke-hook))
        ((eq this-command 'newline)
         (run-hooks 'typewriter-carriage-return-hook)))
  (when (and typewriter-fill-column
             typewriter-show-chars-remaining)
    (force-mode-line-update)))

(defun typewriter--error-handler (data context caller)
  "Handle read-only errors with a custom unlogged message."
  (if (eq (car data) 'buffer-read-only)
      (let ((message-log-max nil))
        (message "You're in typewriter mode.  M-x typewriter-quit to get out to text-mode"))
    (command-error-default-function data context caller)))

(defun typewriter--confirm-exit ()
  "Require confirmation before changing out of `typewriter-mode'."
  (if (y-or-n-p "Exit typewriter mode? ")
      (setq buffer-read-only nil)
    (keyboard-quit)))

(defun typewriter-quit ()
  "Leave `typewriter-mode' and enter `text-mode'."
  (interactive)
  (text-mode))

(defvar-keymap typewriter-mode-map
  :doc "Keymap for `typewriter-mode'."
  "RET" #'newline
  "<return>" #'newline
  "TAB" #'typewriter-tab
  "<tab>" #'typewriter-tab
  "DEL" #'typewriter-backward-char
  "<delete>" #'typewriter-backward-char
  "<backspace>" #'typewriter-backward-char)

(define-derived-mode typewriter-mode fundamental-mode "Typewriter"
  "A major mode emulating a strict typewriter.

Only allows appending text to the end of the buffer or on whitespace.
No deletions or arbitraty edits."
  (electric-indent-local-mode -1)
  (unless typewriter-preserve-undo-history
    (setq-local buffer-undo-list t))
  (setq-local buffer-read-only t
              ;; force emacs's native tab system to behave like a
              ;; mechanical tab
              indent-line-function #'tab-to-tab-stop
              tab-width typewriter-tab-width
              tab-stop-list nil
              command-error-function #'typewriter--error-handler)
  (add-hook 'change-major-mode-hook #'typewriter--confirm-exit nil t)
  (add-hook 'pre-command-hook #'typewriter--pre-command nil t)
  (add-hook 'post-command-hook #'typewriter--post-command nil t)
  (unless (member '(:eval (typewriter--modeline-remaining)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  '((:eval (typewriter--modeline-remaining)))))))

(provide 'typewriter)

;;; typewriter.el ends here
