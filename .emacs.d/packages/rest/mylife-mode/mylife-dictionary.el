;;; --- Helper methods for looking up word definitions
;;

(require 'url-util)
(require 'popup)
(require 'xml)

(defcustom mylife-webster-dictionary-key
  "DICTIONARY-KEY"
  "Key for accessing the Merriam-Webster's collegiate dictionary."
  )

(defcustom mylife-webster-thesaurus-key
  "THESAURUS-KEY"
  "Key for accessing the Merriam Webster's Thesaurus.")

(defvar mylife-webster-base-url
  "http://www.dictionaryapi.com/api/v1/references"
  "Base url for the Webster API")

(defun mylife-webster-build-url (word reference)
  "Builds the url end point for finding WORD in REFERENCE."
  (url-encode-url (format "%s/%s/xml/%s?key=%s"
                          mylife-webster-base-url
                          reference
                          word
                          (pcase reference
                            ("collegiate" mylife-webster-dictionary-key)
                            ("thesaurus" mylife-webster-thesaurus-key)
                            (n (error "Invalid name of the refernce"))))))

(defun mylife-webster-fetch-parse-do (url parser action)
  "Fetch the page for the `url` and parse it with parser and do action on 
parsed value. `parser` parses the contents of the buffer from point "
  (let ((url-request-method "GET")
        (url-callback `(lambda (status)
                         (if status
                             (message "%s" status)
                           (search-forward "\n\n")
                           (,action (,parser))))))
    (message "Fetching %s" url)
    (url-retrieve url url-callback)))

(defun mylife-webster-parser-collegiate ()
  "Stringify the xml tree after parsing it from buffer for the
collegiate reference."
  (let* ((root (car  (xml-parse-region (point) (point-max))))
         (entries (xml-get-children root 'entry))
         (first-entry (car entries)))
    (format "%s %s"
            (mylife-webster-parser-et first-entry)
            (mapconcat 'mylife-webster-parser-entry
                       entries
                       "\n"))))

(defun mylife-webster-parser-entry (entry)
  "Returns the string representation of an etry node"
  (format "%s\n"
          (mylife-webster-parser-dt entry)))

(defun mylife-webster-parser-dt (entry)
  "Return a string for all dt tags in entry."
  (format "Definition: \n%s"
          (mapconcat
           (lambda
             (dt)
             (format "%s" (xml-node-children dt)))
           (xml-get-children (car (xml-get-children entry 'def )) 'dt)
           " ")))

(defun mylife-webster-parser-et (entry)
  "String representation of the etymology tag of entry."
  (format "Etymology: \n%s "
          (mapconcat
           (lambda (w)
             (format "%s" w))
           (xml-node-children (xml-node-name (xml-get-children entry 'et )))
           " ")))

(defun mylife-define-word (word)
  "Provide definition for `word`."
  (mylife-webster-fetch-parse-do
   (mylife-webster-build-url word "collegiate")
   'mylife-webster-parser-collegiate
   'message))

(provide 'mylife-dictionary)

(mylife-define-word "life")
