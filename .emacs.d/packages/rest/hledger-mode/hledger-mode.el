;;; hledger-mode.el -- A mode for writing journal entries for hledger
;; Author: Narendra Joshi <narendraj9@gmail.com>
;; #TODO

(require 'json)

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(defgroup hledger nil
  "Customization group hledger-mode.")

(defcustom hledger-mode-hook nil
  "Normal hook for entering hledger-mode."
  :type 'hook
  :group 'hledger)

(defcustom hledger-jfile "~/miscellany/personal/finance/accounting.journal"
  "Location of the journal file."
  :group 'hledger
  :type 'string)

(defcustom hledger-comments-column 11
  "Column number where the comments start."
  :group 'hledger
  :type 'integer)

(defvar hledger-currency-string "₹"
  "String to be used for currency. Assumes it is prefixed.")

(defvar hledger-service-fetch-url
  "Service url for fetching journal entries."
  nil)

;;; Regexes
(defvar hledger-empty-regex "^\\s-*$"
  "Regular expression for an empty line.")
(defvar hledger-date-only-regex "^\\s-*[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-*$"
  "Regular expression a line with date only.")
(defvar hledger-date-regex "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}"
  "Regular expression for dates for font lock.")
(defvar hledger-date-and-desc-regex "\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-*[^[:space:]]+\\>"
  "Regular expression for matching a starting entry with some description.")
(defvar hledger-account-regex "\\(\\(assets\\|liabilities\\|equity\\|expenses\\|income\\|zadjustments\\)\\(:[a-z--]+\\)*\\)"
  "Regular expression for a potential journal account.")
(defvar hledger-whitespace-account-regex "\\s-*\\(\\(assets\\|liabilities\\|equity\\|expenses\\|income\\|zadjustments\\)\\(:[a-z--]+\\)*\\)"
  "Regular expression for an account with leading whitespace.")
(defvar hledger-comment-regex "^[ \t]*;"
  "Regular expression for a comment in journal file.")
(defvar hledger-empty-comment-regex "^\\s-*;\\s-*$"
  "Regular expression to match a comment with no text.")
(defvar hledger-amount-regex (format "\\<%s [-]?[0-9]+\\(\\.[0-9]+\\)?\\>"
                                     hledger-currency-string)
  "Regular expression to match an inserted amount in rupees.")

;;; Indentation
(defun hledger-line-matchesp (re offset)
  "Returns a boolean value stating whether the line OFFSET 
lines above the current line starts with the regular experssion
RE."
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at re)))

;; Internal functions for looking-at lines' beginnings
(defun hledger-cur-line-matchesp (re)
  "Returns true if current line starts with RE."
  (hledger-line-matchesp re 0))
(defun hledger-prev-line-matchesp (re)
  "Returns true if previous line matches RE."
  (hledger-line-matchesp re -1))

;; Auxiliary funtions[s]
(defun hledger-delete-cur-line ()
  "Delete the current line."
  (delete-region (line-beginning-position) (line-end-position)))
(defun hledger-insert-date ()
  "Insert date at point."
  (insert (format-time-string "%Y-%m-%d ")))
(defun hledger-insert-comment ()
  "Insert a comment on the current line."
  (indent-line-to hledger-comments-column)
  (insert "; "))
(defun hledger-insert-rupee ()
  "Insert the amount for a transaction in hledger"
  (beginning-of-line)
  (re-search-forward hledger-whitespace-account-regex)
  (insert "    ₹ "))
(defun hledger-delete-rupee-sign ()
  "Delete the rupee sign."
  (beginning-of-line)
  (re-search-forward hledger-whitespace-account-regex
                     (line-end-position)
                     t)
  (delete-region (point) (line-end-position)))

(defun hledger-acc-line-has-rupeep ()
  "Returns true if the account line has an amount."
  (hledger-cur-line-matchesp (concat hledger-whitespace-account-regex
                                     (format "\\s-*%s\\s-*$" 
                                             hledger-currency-string))))
(defun hledger-expecting-rupeep ()
  "Returns true if we should insert a rupee sign."
  (hledger-cur-line-matchesp (concat hledger-whitespace-account-regex
                                     "\\s-*$")))

(defun hledger-cur-line-emptyp ()
  "Returns true if current line is empty."
  (hledger-cur-line-matchesp hledger-empty-regex))
(defun hledger-cur-has-datep ()
  "Returns true if current line has only date."
  (hledger-cur-line-matchesp hledger-date-only-regex))
(defun hledger-cur-has-empty-commentp ()
  "Returns true if current line has an empty comment. Empty comments."
  (hledger-cur-line-matchesp hledger-empty-comment-regex))
(defun hledger-cur-has-accp ()
  "Returns true if the current line has an account name."
  (hledger-cur-line-matchesp hledger-whitespace-account-regex))
(defun hledger-cur-starts-with-semicolp ()
  "Returns true if the current line starts with a semicolon."
  (hledger-cur-line-matchesp hledger-comment-regex))

(defun hledger-prev-line-emptyp ()
  "Returns true if previous line is empty."
  (hledger-prev-line-matchesp hledger-empty-regex))
(defun hledger-prev-has-datep ()
  "Returns true if previous line has date and description."
  (hledger-prev-line-matchesp hledger-date-and-desc-regex))
(defun hledger-prev-has-commentp ()
  "Returns true if previousl line has an empty comment. Empty or otherwise."
  (hledger-prev-line-matchesp hledger-comment-regex))
(defun hledger-prev-has-accp ()
  "Returns true if the previous line has an account name."
  (hledger-prev-line-matchesp hledger-whitespace-account-regex))
  
(defun hledger-indent-empty-line ()
  "Called when the line to be indented is an empty one."
  (cond
   ((hledger-prev-line-emptyp)   (hledger-insert-date))
   ((hledger-prev-has-datep) (if (= (current-indentation) tab-width)
                                 (hledger-insert-comment)
                               (hledger-delete-cur-line)
                               (indent-line-to tab-width)))
   ((hledger-prev-has-commentp) (hledger-insert-comment))
   ((hledger-prev-has-accp)
    (indent-line-to tab-width))))

(defun hledger-indent-date-line ()
  "Called when current line has only a date in the beginning."
  (hledger-delete-cur-line))

(defun hledger-indent-comment-line ()
  "Called when current line has an empty comment already."
  (if (not (hledger-cur-has-empty-commentp))
      (indent-line-to hledger-comments-column)
    (hledger-delete-cur-line)
    (indent-line-to tab-width)))

(defun hledger-indent-account-line ()
  "Called when the line to indent is an account listing line."
  (cond
   ((hledger-acc-line-has-rupeep) (hledger-delete-rupee-sign))
   ((hledger-expecting-rupeep) (hledger-insert-rupee))))

(defun hledger-indent-line ()
  "Indent the current line."
  (cond
   ((hledger-cur-line-emptyp) (hledger-indent-empty-line))
   ((hledger-cur-has-datep) (hledger-indent-date-line))
   ((hledger-cur-starts-with-semicolp) (hledger-indent-comment-line))
   ((hledger-cur-has-accp) (hledger-indent-account-line))))
   
;;; Auto-complete
(defun hledger-source-init ()
  "Initialize the candidates list for account completion."
  (if (eq major-mode 'hledger-mode)
      (setq-local hledger-jfile (buffer-file-name)))
  (let*
      ((accounts-string (shell-command-to-string
                         (concat "hledger -f" hledger-jfile " accounts")))
       (accounts-list (split-string accounts-string)))
    (setq hledger-source-cache accounts-list)))
(hledger-source-init)

(defvar ac-source-hledger-source
  '((init . hledger-source-init)
    (candidates . hledger-source-cache))
  "A source for completing account names in a hledger buffer.")

;;; Utility functions

(defun hledger-get-perfin-buffer (&optional editable-p)
  "Get/create the *Personal Finance* buffer.
If the buffer is not intended for editing, then `q` closes it.
`C-c y` copies the whole buffer to clipboard. "
  (let ((jbuffer (get-buffer-create "*Personal Finance*")))
    (with-current-buffer jbuffer
      (hledger-mode)
      (or editable-p 
          (local-set-key (kbd "q")
                         (lambda ()
                           (interactive)
                           (quit-restore-window (selected-window) 'kill))))
      (local-set-key (kbd "C-c y")
                     (lambda ()
                       (interactive)
                       (clipboard-kill-ring-save (point-min) (point-max))
                       (message "Buffer copied to clipboard")))
      (erase-buffer))
    jbuffer))

(defun hledger-fetch-entries-callback (status)
  (search-forward "\n\n")
  (let ((entries (append (json-read) nil))
        (result ""))
    (dolist (entry entries)
      (let ((description (cdr (assoc 'description entry)))
            (comment (cdr (assoc 'comment entry)))
            (postings (cdr (assoc 'postings entry)))
            (date (format-time-string "%Y-%m-%d")))
        (setf result 
              (concat result 
                      (format "%s %s\n" 
                              date 
                              description)))
        (dolist (posting (append postings nil))
          (let ((account (cdr (assoc 'account posting)))
                (amount (cdr (assoc 'amount posting))))
            (setf result
                  (concat result
                          (format "    %s    %s %s\n" 
                                  account
                                  hledger-currency-string
                                  amount))))))
      (setf result (concat result "\n")))
          
    (let ((jbuffer (hledger-get-perfin-buffer t)))
      (with-current-buffer jbuffer
        (insert result))
      (pop-to-buffer jbuffer))))

(defun hledger-fetch-entries ()
  "Fetch journal entries from `hledger-service-url`.
Show the results in the *Personal Finance* buffer"
  (interactive)
  (with-current-buffer 
      (let ((url-request-method "GET")
            (url-debug "all"))
        (url-retrieve (url-generic-parse-url hledger-service-fetch-url)
                      'hledger-fetch-entries-callback))))


(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance"
                                 "register" "incomestatement" "balancesheet"
                                 "cashflow" "activity" "stats")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defun hledger-ask-and-save-buffer ()
  "Ask for saving modified buffer before any reporting commands."
  (if (and (eq major-mode 'hledger-mode)
           (buffer-modified-p)
           (yes-or-no-p (format "Save buffer %s? "
                                (buffer-name))))
      (save-buffer)
    (ignore)))

(defun hledger-go-to-starting-line ()
  "Function to go the first line that stars a new entry."
  (goto-char (point-max))
  (beginning-of-line)                   
  (while (looking-at hledger-empty-regex) 
    (forward-line -1))                    
  (end-of-line)
  (delete-region (point) (point-max))
  (insert "\n\n"))

(defun hledger-overlay-current-entry ()
  "Engulf an entry in an overlay."
  (interactive)
  (while (and (not (bobp))
              (not (looking-at hledger-empty-regex)))
    (forward-line -1))
  (forward-line 1)
  (setq begin (point))
  (forward-line 1)
  (while (and (not (looking-at hledger-empty-regex))
              (not (eobp)))
    (forward-line 1))
  (setq end (point))
  (setq current-entry (make-overlay begin end))
  (overlay-put current-entry 'face '(:background "black")))

(defun hledger-clear-undo-list ()
  "Empty `buffer-undo-list`."
  (buffer-disable-undo)     
  (buffer-enable-undo))     

(defun hledger-eval-region (beg end)
  "Send selected region to hledger for evaluation."
  (interactive "r")
  (let ((command (completing-read "jdo> " hledger-jcompletions))
        (hledger-jfile (make-temp-file "hledger")))
    (write-region beg end hledger-jfile)
    (hledger-jdo command)))

(defun hledger-jentry ()
  "Make a new entry in the financial journal. Avoids editing old entries."
  (interactive)
  (find-file hledger-jfile)
  (hledger-go-to-starting-line)
  (recenter))

(defun hledger-jdo (command)
  "Run a hledger command on the journal file."
  (interactive (list (completing-read "jdo> " hledger-jcompletions)))
  (hledger-ask-and-save-buffer)
  (if (eq major-mode 'hledger-mode)
      (setq-local hledger-jfile (buffer-file-name)))
  (let ((jbuffer (hledger-get-perfin-buffer))
        (jcommand (concat "hledger -f " hledger-jfile " " command)))
    (with-current-buffer jbuffer
      (call-process-shell-command jcommand nil t nil)
      (pop-to-buffer jbuffer))))
      
(defun hledger-jreg (pattern)
  "Run hledger register command."
  (interactive "spattern> ")
  (let ((jcmd (concat "register " pattern)))
    (hledger-jdo jcmd)))

(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "RET")
      (lambda ()
        (interactive)
        (newline-and-indent)))
    (define-key map (kbd "<backtab>")
      (lambda ()
        (interactive)
        (backward-delete-char-untabify tab-width)))
    map))

(defconst hledger-font-lock-keywords-1
  (list
   `(,hledger-account-regex . font-lock-variable-name-face)
   `(,hledger-date-regex . font-lock-string-face)
   `(,hledger-amount-regex . font-lock-constant-face))
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-defaults '(hledger-font-lock-keywords-1)
  "Default highlighting expressions for hledger mode")

(defvar hledger-mode-syntax-table (let ((st (make-syntax-table)))
                                    (modify-syntax-entry ?: "_" st)
                                    (modify-syntax-entry ?; "<" st)
                                    (modify-syntax-entry ?\n ">" st)
                                    st)
  "Syntax table for hledger mode.")

(defun hledger-mode-init ()
  "Function that does initial setup in the major-mode function."
  (use-local-map hledger-mode-map)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-line)
  (setq-local ac-sources '(ac-source-hledger-source))
  (electric-indent-local-mode -1))
    
;;;###autoload
(define-derived-mode hledger-mode prog-mode "HLedger" ()
  "Major mode for editing hleder mode files."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (hledger-mode-init))

(provide 'hledger-mode)
