;;; mylife-quotes.el --- Quotes                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: wp

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

;; For displaying a random quote on Emacs startup. I use it for
;; maintianing a list of quotes as well.

;;; Code:

(require 'utils)

(defcustom mylife-quote-face
  '(:foreground "LemonChiffon" :height 1.2)
  "Face for a quote."
  :group 'mylife-mode
  :type 'face)

(defconst mylife-script-directory
  (expand-file-name (file-name-directory load-file-name))
  "The directory that this script is kept in.")

(defcustom mylife-personal-quotes-file
  (expand-file-name "_assets/mylife-quotes.txt" mylife-script-directory)
  "Path to the custom quotes file. Must have quotes separated by a newline.")

(defcustom mylife-quotes
  (ignore-errors
   (mapcar
    (lambda (quote)
      (propertize (format "%s\n\n" quote)
                  'font-lock-face mylife-quote-face
                  'rear-nonsticky t))
    (with-temp-buffer
      (insert-file-contents mylife-personal-quotes-file)
      (goto-char (point-min))
      (split-string
       (buffer-substring-no-properties (point-min) (point-max))
       "\n\n"))))
  "Quotes from Conal Elliot's website and more. Ignore if an error occurs 
while parsing the quotes file"
  
  :group 'mylife-mode
  :type '(repeat string))

(defun mylife-random-quote-string ()
  "Returns a random quote."
  (if mylife-quotes
      (nth (random (length mylife-quotes)) mylife-quotes)
    (message "No quotes defined. Maybe the quotes file wasn't parsed properly")))

(defun mylife-prepare-quote (quote &optional author)
  "Prepare a nicely formatted quote from the arguments."
  (let* ((author-line-space-count  (- fill-column (length author) 2))
         (author-line-string (format "%s - %s"
                                     (if (< author-line-space-count 0)
                                         ""
                                       (make-string author-line-space-count ? ))
                                     (or author "Unknown"))))
    (format "“%s”\n%s" quote author-line-string)))

(defun mylife-add-new-quote (quote &optional author)
  "Adds a new quote to the list of quotes. 
Turn mylife-quotes into a variable maintained with `customize-save-variable`."
  (interactive "sQuote: \nsAuthor: ")
  (with-current-buffer (find-file-noselect mylife-personal-quotes-file)
    (utils-go-to-starting-line)
    (insert (mylife-prepare-quote quote author))
    (save-buffer)
    (kill-buffer)))

(defun mylife-qod-callback (status)
  "Callback for mylife-fetch-qod command. This currently replaces the contents
of the *scratch* buffer with the quote string."
  (search-forward "\n\n")
  (if (not status)
    (let* ((quote-json (json-read))
           (quotes (assoc-default
                    'quotes (assoc-default
                             'contents quote-json)))
           (quote (aref quotes 0))
           (quote-string (assoc-default 'quote quote))
           (quote-author (assoc-default 'author quote))
           (quote-img-url (assoc-default 'background quote)))
      (kill-buffer)
      (with-current-buffer (get-buffer-create "*scratch*")
        (erase-buffer)
        (insert (propertize (mylife-prepare-quote quote-string quote-author)
                            'font-lock-face mylife-quote-face
                            'rear-nonsticky t))
        (goto-char (point-max))
        (insert "\n\n")
        (message "Quote in *scratch*")))
    (message "Error fetching quote: %s"
             (assoc-default 'message
                            (assoc-default 'error (json-read))))))

(defun mylife-fetch-qod ()
  "Fetches quote of the day from theysaidso.com"
  (interactive)
  (setq qod-result "")
  (with-current-buffer 
      (let ((url-request-method "GET")
            (qod-service-url "http://quotes.rest/qod.json"))
        (url-retrieve (url-generic-parse-url qod-service-url)
                      'mylife-qod-callback))))

(defun mylife-refresh-scratch-buffer (&optional pop-to-bufferp)
  "Recreate and refresh the scracth buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((here-marker (point-marker))
          (inhibit-read-only t)
          (content (concat (mylife-random-quote-string)
                           (mylife-get-auroville-quality))))
      (when (not (= emacs-scratch-text-size 0))
        (delete-region (point-min) (min (point-max)
                                        emacs-scratch-text-size)))
      (goto-char (point-min))
      (insert content)
      (setq emacs-scratch-text-size (point))
      (font-lock-mode 1)
      (goto-char (marker-position here-marker))
      (and pop-to-bufferp (pop-to-buffer "*scratch*")))))

(provide 'mylife-quotes)
