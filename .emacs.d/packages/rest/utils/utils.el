;;; -- A set of functions common to other modes written here

(defun utils-go-to-starting-line ()
  "Function to go the first line that stars a new entry for anything. 
Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)                   
  (while (looking-at "^\\s-*$") 
    (forward-line -1))                    
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (dotimes (i times-yet-to-move)
      (insert "\n"))))

;; Helpful Paredit functions
(defun utils-paredit-barf-all-the-way-backward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

(defun utils-paredit-barf-all-the-way-forward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-forward-down)
  (paredit-splice-sexp)
  (if (eolp) (delete-horizontal-space)))

(defun utils-paredit-slurp-all-the-way-backward ()
  (interactive)
  (catch 'done
    (while (not (bobp))
      (save-excursion
        (paredit-backward-up)
        (if (eq (char-before) ?\()
            (throw 'done t)))
      (paredit-backward-slurp-sexp))))

(defun utils-paredit-slurp-all-the-way-forward ()
  (interactive)
  (catch 'done
    (while (not (eobp))
      (save-excursion
        (paredit-forward-up)
        (if (eq (char-after) ?\))
            (throw 'done t)))
      (paredit-forward-slurp-sexp))))

(defun utils-easy-move (map)
  "Set key-binding for easier navigation. Use when in read-only/view-mode."
  (interactive)
  (define-key map (kbd "h") 'backward-char)
  (define-key map (kbd "l") 'forward-char)
  (define-key map (kbd "j") 'next-line)
  (define-key map (kbd "k") 'previous-line)
  map)

(defun utils-uneasy-move (map)
  "Reset key bindings set by easy-move."
  (interactive)
  (define-key map (kbd "h"))
  (define-key map (kbd "l"))
  (define-key map (kbd "j"))
  (define-key map (kbd "k"))
  map)

(defun utils-make-multipart-boundary ()
  "Make the boundary for multipart/form-data
.Creates some slightly unprobably gibberish."
  (concat "x" (make-string 18 ?-) (format "%x" (random 99999999999))))

(defun utils-make-multipart-url-data (boundary params)
  "Construct a multipart/form-data body string with BOUNDARY and PARAMS."
  (concat
   (mapconcat (lambda (kv)
                (let* ((name (format "%s" (car kv)))
                       (value (cdr kv))
                       (encoded-value (encode-coding-string value 'utf-8)))
                  (concat (concat "--" boundary) "\r\n"
                          "Content-Disposition: form-data; "
                          "name=\"" name "\"\r\n\r\n"
                          encoded-value "\r\n")))
              params
              "")
   "--" boundary "--\r\n"))

(defun utils-send-email-with-mailgun (url headers)
 "Send email using Mailgun.

This function emulates the curl command as available in the Mailgun Docs:
curl -s --user USER-AND-PASSWD URL 
 -F FROM='Excited User <excited@samples.mailgun.org>' \
 -F TO='devs@mailgun.net' \
 -F SUBJECT='Hello' \
 -F TEXT='Testing some Mailgun awesomeness!'

HEADERS is an assoc-list with the headers of the request.
`((authorization . AUTHORIZATION)
  (from . FROM)
  (to   . TO)
  (subject . SUBJECT)
  (text . TEXT))
"
(let* ((multipart-boundary (utils-make-multipart-boundary))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Content-Type" . ,(format
                                "multipart/form-data; boundary=%s; charset=utf-8"
                                multipart-boundary))
            ("Authorization" . ,(concat
                                 "Basic "
                                 (base64-encode-string
                                  (assoc-default 'authorization headers))))))
         (url-request-data
          (utils-make-multipart-url-data multipart-boundary
                                         (assq-delete-all 'authorization headers))))
    (url-retrieve url
                  (lambda (status)
                    (if status
                        (message "Failed with: %s" status)
                      (search-forward "\n\n")
                      (message "%s" (assoc-default 'message (json-read))))
                    (kill-buffer)))))

(defun utils-send-text-email (url user-and-password from to subject text)
  "Send an email with text body.  
URL is the api-endpoint [Mailgun HTTP API endpoint].
USER-AND-PASSWORD is in the format 'user:password' and is
base64-encoded to make the Authorization header for simple
authentication. The rest of the fields have their obvious
objectives. "
  (utils-send-email-with-mailgun url `((authorization . ,user-and-password)
                                       (from . ,from)
                                       (to . ,to)
                                       (subject . ,subject)
                                       (text . ,text))))

(defun utils-send-html-email (url user-and-password from to subject html)
  "Send an email with HTML body.
See `utils-send-text-email'."
  (utils-send-email-with-mailgun url `((authorization . ,user-and-password)
                                       (from . ,from)
                                       (to . ,to)
                                       (subject . ,subject)
                                       (html . ,html))))

;; (defun utils-send-email-with-mailgun (url user-and-passwd from to subject text)
;;   "Send email using Mailgun.
;; This function emulates the curl command as available in the Mailgun Docs:
;; curl -s --user USER-AND-PASSWD URL 
;;  -F FROM='Excited User <excited@samples.mailgun.org>' \
;;  -F TO='devs@mailgun.net' \
;;  -F SUBJECT='Hello' \
;;  -F TEXT='Testing some Mailgun awesomeness!'
;; "
;;   (let* ((multipart-boundary (utils-make-multipart-boundary))
;;          (url-request-method "POST")
;;          (url-request-extra-headers
;;           `(("Content-Type" . ,(format
;;                                 "multipart/form-data; boundary=%s; charset=utf-8"
;;                                 multipart-boundary))
;;             ("Authorization" . ,(concat
;;                                  "Basic "
;;                                  (base64-encode-string user-and-passwd)))))
;;          (url-request-data
;;           (utils-make-multipart-url-data multipart-boundary
;;                                          `(("from" . ,from)
;;                                            ("to" . ,to)
;;                                            ("subject" . ,subject)
;;                                            ("text" . ,text)))))
;;     (url-retrieve url
;;                   (lambda (status)
;;                     (if status
;;                         (message "Failed with: %s" status)
;;                       (search-forward "\n\n")
;;                       (message "%s" (assoc-default 'message (json-read))))
;;                     (kill-buffer)))))

;;;###autoload 
(define-minor-mode utils-easy-move-mode
  "A mode for bindings in a read only environment. Mimicks vim."
  nil
  "Easy"
  (utils-easy-move (make-sparse-keymap)))

(provide 'utils)
