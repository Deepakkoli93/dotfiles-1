;;; gnus.el --- My Gnus configuration                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: mail

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

;; It's Gnus.

;;; Code:

(require 'bbdb)
(require 'bbdb-gnus)

(require 'gnus)
(require 'gnus-async)

(require 'nnmail)
(require 'smtpmail)
(require 'message)

(setq user-mail-address "narendraj9@gmail.com"
      user-full-name "Narendra Joshi"
      message-signature "Narendra Joshi")


(setq gnus-select-method
      `(nnimap "gmail"
               (nnimap-address "imap.gmail.com")
               (nnimap-inbox "INBOX")
               (nnimap-stream ssl)
               (nnimap-split-methods default)))


(setq smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")


;; http://groups.google.com/group/gnu.emacs.gnus/browse_thread/thread/a673a74356e7141f
(when window-system
  (setq gnus-thread-hide-subtree t
        gnus-sum-thread-tree-indent "  "
        gnus-sum-thread-tree-root "● "
        gnus-sum-thread-tree-false-root "◯ "
        gnus-sum-thread-tree-single-indent "◎ "
        gnus-sum-thread-tree-vertical        "│"
        gnus-sum-thread-tree-leaf-with-other "├─► "
        gnus-sum-thread-tree-single-leaf     "╰─► "))

;; Article and thread sorting
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-most-recent-date
        (not gnus-thread-sort-by-number)))

(setq gnus-user-date-format-alist '((t . "%Y-%m-%d %H:%M"))
      gnus-summary-line-format (concat
                                "%0{%U%R%z%}"
                                "%3{│%}" "%1{%&user-date;%}" "%3{│%}" ;; date
                                "  "
                                "%4{%-20,20f%}" ;; name
                                "  "
                                "%3{│%}"
                                " "
                                "%1{%B%}"
                                "%s\n"))

(setq gnus-summary-display-arrow t)

;; INBOX splitting
(setq nnmail-split-methods
      '(("GNU" "^To:.*gnu\\.org")
        ("GNU" "^Cc:.*gnu\\.org")
        ("Primary" "")))

;; Pre-fetch for speed
(setq gnus-asynchronous t)

;; HTML Email -> Text
(setq mm-text-html-renderer 'gnus-w3m)

(provide 'gnus)
;;; gnus.el ends here
