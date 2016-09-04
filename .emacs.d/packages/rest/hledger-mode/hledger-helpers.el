;;; hledger-helpers.el --- Helper functions for hledger-mode.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains code for functions defined to make
;; hledger-mode.el simpler and shorter.

;;; Code:

(defun hledger-ret-command ()
  "Commands run on <return> in hledger-mode."
  (interactive)
  (newline-and-indent))

(defun hledger-backtab-command ()
  "Commands runon <backtab> in hledger-mode."
  (interactive)
  (backward-delete-char-untabify tab-width))

(defun hledger-kill-reporting-window ()
  "Kills the reporting buffer and window."
  (interactive)
  (if (>= (length (window-list)) 2)
      (kill-buffer-and-window)
    (kill-buffer)))

(defun hledger-copy-to-clipboard ()
  "Copies current buffer contents to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard"))

(defun hledger-append-clipboard-to-journal ()
  "Appends clipboard contents to journal file."
  (interactive)
  (let ((entries (buffer-string)))
    (hledger-jentry)
    (insert entries)
    (format "Fetched entries appended.")))

(defmacro hledger-as-command (command)
  "Wrapper macro for interactive key bindings."
  `(lambda () (interactive) (hledger-run-command ,command)))

(provide 'hledger-helpers)
;;; hledger-helpers.el ends here
