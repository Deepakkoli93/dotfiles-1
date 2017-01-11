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

;; For displaying a random quote on Emacs startup.  I use it for
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
  "Path to the custom quotes file.  Must have quotes separated by a newline.")

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
  "Quotes from Conal Elliot's website and more.
Ignore if an error occurs while parsing the quotes file"

  :group 'mylife-mode
  :type '(repeat string))

(defun mylife-random-quote-string ()
  "Return a random quote."
  (if mylife-quotes
      (nth (random (length mylife-quotes)) mylife-quotes)
    (message "No quotes defined. Maybe the quotes file wasn't parsed properly")))

(defun mylife-prepare-quote (quote &optional author)
  "Prepare a nicely formatted QUOTE from the arguments.
Optional argument AUTHOR is the name of the author."
  (let* ((author-line-space-count  (- fill-column (length author) 2))
         (author-line-string (format "%s - %s"
                                     (if (< author-line-space-count 0)
                                         ""
                                       (make-string author-line-space-count ? ))
                                     (or author "Unknown"))))
    (format "“%s”\n%s" quote author-line-string)))

(defun mylife-add-new-quote (quote &optional author)
  "Add a new QUOTE to the list of quotes.
Turn ‘mylife-quotes’ into a variable maintained with `customize-save-variable`.
Optional argument AUTHOR is what the word suggests but checkdoc was complaining so this sentence."
  (interactive "sQuote: \nsAuthor: ")
  (with-current-buffer (find-file-noselect mylife-personal-quotes-file)
    (utils-go-to-starting-line)
    (insert (mylife-prepare-quote quote author))
    (save-buffer)
    (kill-buffer)))

(defun mylife-qod-callback (status)
  "Callback for ‘mylife-fetch-qod’ command.
This currently replaces the contents
of the *scratch* buffer with the quote string.
Argument STATUS is the http status of the request."
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
  "Fetches quote of the day from theysaidso.com."
  (interactive)
  (setq qod-result "")
  (with-current-buffer
      (let ((url-request-method "GET")
            (qod-service-url "http://quotes.rest/qod.json"))
        (url-retrieve (url-generic-parse-url qod-service-url)
                      'mylife-qod-callback))))

(defun mylife-generate-scratch-message ()
  "Generate message content for scratch buffer.
Make sure you set the :text-type text property to :quote-string."
  (propertize (concat (mylife-random-quote-string)
                      (mylife-get-auroville-quality))
              :text-type :quote-string))

(defun mylife--remove-text-with-property (start p v)
  "From point START, remove first chunk with prop P set to V.
This function returns the point value for the second of the
deleted text so that it can be called again with that value to
delete all text in a buffer."
  (let* ((beg (text-property-any start (point-max) p v))
         (end (and beg
                   (text-property-not-all beg (point-max) p v))))
    (and beg ; there is some text
         (delete-region beg (or end ; it's all of the text
                                (point-max))))
    beg))

(defun mylife-refresh-scratch-buffer (&optional pop-to-bufferp)
  "Recreate and refresh the scracth buffer.
Optional argument POP-TO-BUFFERP makes the window pop to the buffer if non-nil."
  (interactive)
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((quote-visible-p (pos-visible-in-window-p (point-min)))
          (here-marker (point-marker))
          (inhibit-read-only t)
          ;; Distinguishing quote text from other text with a text
          ;; property.
          (content (mylife-generate-scratch-message)))
      ;; Advance marker when we insert text at its position
      (set-marker-insertion-type here-marker t)

      ;; I might have fragmented the quote text in which case, I would
      ;; like to work only on the quote text and not change the other
      ;; unrelated text in the scratch buffer.
      (while (mylife--remove-text-with-property (point-min)
                                                :text-type
                                                :quote-string))
      ;; Now insert new quote at the top of the buffer
      (goto-char (point-min))
      (insert content)
      (font-lock-mode 1)
      (goto-char (marker-position here-marker))
      (when quote-visible-p
        (set-window-start (selected-window) (point-min)))
      (and pop-to-bufferp (pop-to-buffer "*scratch*")))))


(provide 'mylife-quotes)

;;; mylife-quotes.el ends here
