;;; mylife-dictionary.el --- Word lookup using Pearson API  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, tools

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

;; 

;;; Code:

(require 'json)
(require 'url-util)
(require 'popup)

(defcustom mylife-pearson-consumer-key
  "DICTIONARY-KEY"
  "Key for accessing the Pearson's dictionaries.")

(defcustom mylife-pearson-results-limit
  6
  "The number of results to fetch.")

(defvar mylife-pearson-base-url
  "http://api.pearson.com"
  "Base url for the Pearson API")

(defvar mylife-webster-base-url
  "http://www.dictionaryapi.com/api"
  "Base url for the Merriam-Webster's API.")

(defvar mylife-webster-thesaurus-key
  "THESAURUS-KEY"
  "Key for the Merriam-Webster's Thesaurus.")

(defun mylife-pearson-build-url (word &optional synonymp)
  "Builds the url end point for finding WORD in REFERENCE."
  (url-encode-url (format "%s/v2/dictionaries%s/entries?apikey=%s&limit=%s&%s=%s"
                          mylife-pearson-base-url
                          (if synonymp
                              ""
                            "/ldoce5")
                          mylife-pearson-consumer-key
                          mylife-pearson-results-limit
                          (if synonymp
                              "synonyms"
                            "headword")
                          word)))

(defun mylife-pearson-fetch-parse-do (url parser action)
  "Fetch the page for the `url` and parse it with parser and do action on 
parsed value. `parser` parses the contents of the buffer from point "
  (let* ((url-request-method "GET")
         (query-buffer (current-buffer))
         (query-point (point))
         (url-callback `(lambda (status)
                          (if status
                              (message "%s" status)
                            (search-forward "\n\n")
                            (,action (,parser) ,query-buffer ,query-point))
                          (kill-buffer))))
    (url-retrieve url url-callback)))

(defun mylife-pearson-parser ()
  "Parsers the current buffer from `point` to return a string."
  (let* ((text (buffer-substring-no-properties (point) (point-max)))
         (decoded-text (decode-coding-string text 'utf-8))
         (json-response (json-read-from-string decoded-text))
         (results (assoc-default 'results json-response)))
    (if (equal results [])
        "Couldn't find anything :("
      (mapconcat 'mylife-pearson-parser-result results "\n"))))

(defun mylife-pearson-parser-result (r)
  "Parse a result json object."
  (let* ((headword (assoc-default 'headword r))
         (ipa (assoc-default 'ipa (elt (assoc-default 'pronunciations r) 0)))
         (part-of-speech (assoc-default 'part_of_speech r))
         (senses (mapconcat 'mylife-pearson-parser-sense
                              (assoc-default 'senses r)
                              "\n")))
    (format "%s: %s %s\n%s"
            part-of-speech
            headword
            (mylife-pearson-parser-string-or-empty "(%s)" ipa)
            senses)))

(defun mylife-pearson-parser-string-or-empty (f s)
  "If s is non-empty use f as argument to `format' else return empty string"
  (if (or (not s) (equal s ""))
      ""
    (format f s)))

(defun mylife-pearson-parser-sense (s)
  "Return the string representation for a sense object from Pearson."
  (let* ((definitions (assoc-default 'definition s))
         (examples (assoc-default 'examples s))
         (ces (assoc-default 'collocation_examples s)))
    (format ": %s\n%s%s%s%s"
            (mapconcat 'identity definitions "\n")
            (mylife-pearson-parser-string-or-empty
             "⟶ %s\n"
             (mapconcat
              (lambda (e) (assoc-default 'collocation e))
              ces
              " | "))
            (mylife-pearson-parser-string-or-empty
             " • %s\n" 
             (mapconcat
              (lambda (e)
                (assoc-default 'text (assoc-default 'example e)))
              ces
              "\n"))
            (mylife-pearson-parser-string-or-empty
             " • %s\n"
             (mapconcat
              (lambda (e)
                (assoc-default 'text e))
              examples
              "\n"))
            (mylife-pearson-parser-string-or-empty
             "synonym: %s\n"
             (assoc-default 'synonym s)))))

(defun mylife-pearson-show-popup (s query-buffer query-point)
  "Shows the results in a popup in the buffer query-buffer at the position
query-point."
  (with-current-buffer query-buffer
    (popup-tip s
               :point query-point
               :margin t
               :around t
               :truncate nil
               :scroll-bar t)))

(defun mylife-define-word (word)
  "Provide definition for `word`."
  (mylife-pearson-fetch-parse-do
   (mylife-pearson-build-url word)
   'mylife-pearson-parser
   'mylife-pearson-show-popup))

(defun mylife-find-synonyms (word)
  "Find out synonyms for word.  
Since the pearson api isn't very good at finding synonyms, I am
using the Merriam Webster's API for the task."
  (mylife-pearson-fetch-parse-do
   (url-encode-url (format "%s/v1/references/thesaurus/xml/%s?key=%s"
                           mylife-webster-base-url
                           word
                           mylife-webster-thesaurus-key))
   `(lambda ()
      "The Great Anynymous Parser for the Merriam-Webster's Thesaurus."
      (while (re-search-forward "<it>\\|</it>" nil t)
        (replace-match "" nil nil))
      (goto-char (point-min))
      (search-forward "\n\n")
      (let* ((root (car (xml-parse-region (point) (point-max))))
             (entries (xml-get-children root 'entry))
             (sense (car (xml-get-children (car entries) 'sens)))
             (synonyms (xml-node-children (car (xml-get-children sense
                                                                 'syn))))
             (antonyms (xml-node-children (car (xml-get-children sense
                                                                 'ant)))))
        (format  "%s\n• Synonyms: %s\n• Antonyms: %s"
                 ,word
                 (and synonyms
                      (replace-regexp-in-string "\\(\\|\\)" ""  (car synonyms)))
                 (and antonyms
                      (replace-regexp-in-string "\\(\\|\\)" "" (car antonyms))))))
   'mylife-pearson-show-popup))

(provide 'mylife-dictionary)


