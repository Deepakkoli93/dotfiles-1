;;; mylife-miscellany.el --- Miscellaneous useful functions for everday tasks  -*- lexical-binding: t; -*-

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

;; This file would be the place for anythng that doesn't fit anywhere
;; else but is worth keeping around.

;;; Code:

(defun show-lyrics (title artist)
  "Show lyrics for the TITLE by ARTIST."
  (require 'notifications)
  (interactive "sTitle: \nsArtist: ")
  (if (not (executable-find "glyrc"))
      (notifications-notify :urgency "critical"
                            :title "Lyrics fetch error"
                            :body "glyrc binary not found.")
    (notifications-notify
     :title (format "%s - %s" title artist)
     :timeout 300000
     :body (shell-command-to-string
            (format (concat "glyrc lyrics -Y --verbosity 0 --write stdout "
                            " --fuzzyness 5 "
                            " --title %s --artist %s" )
                    (shell-quote-argument title)
                    (shell-quote-argument artist))))))


  (provide 'mylife-miscellany)
;;; mylife-miscellany.el ends here
