;;; --- Helper methods for looking up word definitions
;;

(require 'url-util)
(require 'json)
(require 'popup)

(defcustom mylife-pearson-consumer-key
  "DICTIONARY-KEY"
  "Key for accessing the Pearson's dictionaries.")

(defcustom mylife-pearson-results-limit
  2
  "The number of results to fetch.")

(defvar mylife-pearson-base-url
  "https://api.pearson.com"
  "Base url for the Pearson API")


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
                            (,action (,parser) ,query-buffer ,query-point)
                            (kill-buffer)))))
    (message "Fetching %s" url)
    (url-retrieve url url-callback)))

(defun mylife-pearson-parser ()
  "Parsers the current buffer from `point` to return a string."
  (let* ((json-response (json-read))
         (results (assoc-default 'results json-response)))
    (if (equal results [])
        (message "Couldn't find anything :(")
      (mapconcat 'mylife-pearson-parser-result results "\n"))))

(defun mylife-pearson-parser-result (r)
  "Parse a result json object."
  (let* ((headword (assoc-default 'headword r))
         (part-of-speech (assoc-default 'part_of_speech r))
         (senses (mapconcat 'mylife-pearson-parser-sense
                              (assoc-default 'senses r)
                              "\n")))
    (format "%s: %s\n%s" part-of-speech headword senses)))

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
    (popup-tip s :point query-point :margin t :nostrip t)))

(defun mylife-define-word (word)
  "Provide definition for `word`."
  (mylife-pearson-fetch-parse-do
   (mylife-pearson-build-url word)
   'mylife-pearson-parser
   'mylife-pearson-show-popup))

(defun mylife-find-synonyms (word)
  "Find out synonyms for word"
  (mylife-pearson-fetch-parse-do
   (mylife-pearson-build-url word t)
   'mylife-pearson-parser
   'mylife-pearson-show-popup))

(provide 'mylife-dictionary)


