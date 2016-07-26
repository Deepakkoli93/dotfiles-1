;;; -*- lexical-binding: t -*-
;;; hledger-mode.el -- A mode for writing journal entries for hledger

;;; Copyright (C) 2015-2016 Narendra Joshi [This is funny.]

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; URL: 
;; Version: 0.1
;; Keywords: hledger
;; Package-Requires: ((json "1.4"))

;;; Commentary:
;;
;; This is a major mode writing hledger journal files. You must have
;; hledger installed to be able to create the reports: balancesheet,
;; income statement, etc.
;;

;;; Code;

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
  :type 'file)

(defcustom hledger-reports-file "~/miscellany/personal/finance/reports.org"
  "Location to the file where to store the monthly reports."
  :group 'hledger
  :type 'file)

(defcustom hledger-reporting-buffer-name "*Personal Finance*"
  "Name of the buffer for showing or working with reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-url "EMAIL_API_URL"
  "Email API end-point."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-password "EMAIL_API_PASSWD"
  "Password for the Email API"
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-user "EMAIL_API_USER"
  "Username for Email API"
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-sender "SENDER_EMAIL_ID"
  "Email id for the sender of your reports."
  :group 'hledger
  :type 'string)

(defcustom hledger-email-api-recipient "RECIPIENT_EMAIL_ID"
  "Email id for the receiver of your reports, i.e. you!"
  :group 'hledger
  :type 'string)

(defcustom hledger-email-reporting-day 15
  "Day of the month for sending email reports.
I am not checking the range. You are own your own. "
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-reporting-retry-interval 300
  "Seconds to wait before retrying to send emails again."
  :group 'hledger
  :type 'integer)

(defcustom hledger-email-next-reporting-time
  (let* ((time (current-time))
         (day (string-to-number (format-time-string "%d" time)))
         (delta-time (days-to-time (- hledger-email-reporting-day
                                      day))))
    (time-add time delta-time))
  "The next time beyond which we must update this variable.
It is updated after an email has been sent to the user.")

(defcustom hledger-comments-column 11
  "Column number where the comments start."
  :group 'hledger
  :type 'integer)

(defvar hledger-currency-string "₹"
  "String to be used for currency. Assumes it is prefixed.")

(defvar hledger-service-fetch-url
  "https://services.vicarie.in/api/entry"
    "Service url for fetching journal entries.")

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
(defvar hledger-amount-regex (format "\\<%s\\s-*[-]?[0-9]+\\(\\.[0-9]+\\)?\\>"
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
(defun hledger-cur-has-date-and-descp
    "Returns tru if current line had date and description."
  (hledger-cur-line-matchesp hledger-date-and-desc-regex))
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
   ((hledger-expecting-rupeep) (hledger-insert-rupee))
   (t (indent-line-to tab-width))))

(defun hledger-indent-line ()
  "Indent the current line."
  (cond
   ((hledger-cur-line-emptyp) (hledger-indent-empty-line))
   ((hledger-cur-has-datep) (hledger-indent-date-line))
   ((hledger-cur-starts-with-semicolp) (hledger-indent-comment-line))
   ((hledger-cur-has-accp) (hledger-indent-account-line))))

(defun hledger-indent-region-function (start end)
  "Function to indent a region. We need a separate function because we
do different stuff while interactively editing an entry."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (beginning-of-line)
      (cond
       ((hledger-cur-has-datep) (indent-line-to 0))
       ((hledger-cur-starts-with-semicolp) (indent-line-to hledger-comments-column))
       ((hledger-cur-has-accp) (indent-line-to tab-width)))
      (forward-line 1))))
    
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

(defvar ac-source-hledger-source
  '((init . hledger-source-init)
    (candidates . hledger-source-cache))
  "A source for completing account names in a hledger buffer.")

(defun company-hledger (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (pcase command
    (`interactive (company-begin-backend 'company-hledger))
    (`prefix (and (eq major-mode 'hledger-mode)
                  (company-grab-symbol)))
    (`candidates
     (remove-if-not
      (lambda (c)
        (string-prefix-p arg c))
      hledger-source-cache))))

;;; Utility functions

(defun hledger-get-perfin-buffer (&optional keep-bufferp fetched-entriesp)
  "Get/create the `hledger-reporting-buffer-name' buffer.
If the buffer is not intended for editing, then `q` closes it.
`C-c y` copies the whole buffer to clipboard. "
  (let ((jbuffer (get-buffer-create hledger-reporting-buffer-name)))
    (with-current-buffer jbuffer
      (hledger-view-mode)
      (if fetched-entriesp
          (progn
            (local-set-key (kbd "C-c i")
                           (lambda ()
                             "Insert buffer contents into `hledger-jfile`"
                             (interactive)
                             (let ((entries (buffer-string)))
                               (call-interactively 'hledger-jentry)
                               (insert entries)
                               (format "Fetched entries append to journal buffer"))))
            (setq header-line-format " C-c i : Insert into journal "))
        (local-set-key (kbd "C-c q")
                       (lambda ()
                         (interactive)
                         (if (>= (length (window-list)) 2)
                             (kill-buffer-and-window)
                           (kill-buffer))))
        (setq header-line-format " C-c q : Quit "))
      (local-set-key (kbd "C-c w")
                     (lambda ()
                       "Copy buffer contents to clipboard"
                       (interactive)
                       (clipboard-kill-ring-save (point-min) (point-max))
                       (message "Buffer copied to clipboard")))
      (setq header-line-format (concat
                                header-line-format
                                " C-c w : Copy to clipboard "))
      (or keep-bufferp (erase-buffer)))
    jbuffer))

(defun hledger-format-comment-string (comment)
  "Format the input COMMENT string for insertion into a journal file."
  (with-temp-buffer (progn
                      (if (string-match-p hledger-empty-regex comment)
                          ""
                        (electric-indent-mode -1)
                        (setq-local fill-column (- 70 hledger-comments-column))
                        (insert comment)
                        (insert "\n")
                        (goto-char (point-min))
                        (fill-paragraph)
                        (setq-local comment-start "; ")
                        (setq-local comment-end "")
                        (comment-region (point-min) (point-max))
                        (indent-region (point-min) (point-max) hledger-comments-column)
                        (buffer-string)))))

(defun hledger-fetch-entries-insert (entries)
  "Insert entries into a journal buffer."
  (interactive)
  (let ((result ""))
    (dolist (entry (reverse entries))
      (let ((description (cdr (assoc 'description entry)))
            (comment (hledger-format-comment-string
                      (cdr (assoc 'comment entry))))
            (postings (cdr (assoc 'postings entry)))
            (date (cdr (assoc 'date entry))))
        (setf result 
              (concat result 
                      (format "%s %s\n%s" 
                              date 
                              description
                              comment)))
        (dolist (posting (append postings nil))
          (let ((account (cdr (assoc 'account posting)))
                (amount (cdr (assoc 'amount posting))))
            (setf result
                  (concat result
                          (format "    %s    %s %s\n" 
                                  account
                                  (if (string-match "[0-9]+" amount)
                                      hledger-currency-string
                                    "" )
                                  amount))))))
      (setf result (concat result "\n")))
    (kill-buffer (current-buffer))
          
    (let ((jbuffer (hledger-get-perfin-buffer nil t)))
      (with-current-buffer jbuffer
        (insert result))
      (pop-to-buffer jbuffer)
      (goto-char (point-min)))))

(defun hledger-fetch-entries ()
  "Fetch journal entries from `hledger-service-url`.
Show the results in the `hledger-reporting-buffer-name' buffer. 
**This is a workaround**.
"
  (interactive)
  (browse-url hledger-service-fetch-url)
  (read-from-minibuffer "Opening browser. Hit [Enter] after copy. ")
  (with-temp-buffer 
    (yank)
    (goto-char (point-min))
    ;; Convert vector returned by json-read to a list
    (let ((entries-list (append (json-read) nil)))
      (kill-buffer)
      (hledger-fetch-entries-insert entries-list)))
  (message "Entries copied"))

(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance"
                                 "register" "incomestatement" "balancesheet"
                                 "cashflow" "activity" "stats"
                                 "monthly-report"
                                 "running-report")
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
  "Function to go the first line that stars a new entry. Cleans up whitespace."
  (goto-char (point-max))
  (beginning-of-line)                   
  (while (looking-at hledger-empty-regex) 
    (forward-line -1))                    
  (end-of-line)
  (let ((times-yet-to-move (forward-line 2)))
    (dotimes (i times-yet-to-move)
      (insert "\n"))))

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

(defun hledger-run-command (command)
  (interactive (list (completing-read "jdo> " hledger-jcompletions)))
  (hledger-ask-and-save-buffer)
  (pcase command
    (`"monthly-report" (hledger-monthly-report))
    (`"running-report" (hledger-running-report))
    (_ (hledger-jdo command))))

(defun hledger-jdo (command &optional keep-bufferp bury-bufferp)
  "Run a hledger command on the journal file.
Returns the buffer with the info inserted.

If KEEP-BUFFERP is non-nil, it won't erase the old contents. New
info would be prepended to the old one.

If BURY-BUFFERP is t, the `hledger-reporting-buffer-name' buffer would not be
showm to the user, this is user for using this function in elisp only
for the buffer contents. "
  (if (eq major-mode 'hledger-mode)
      (setq-local hledger-jfile (buffer-file-name)))
  (let ((jbuffer (hledger-get-perfin-buffer keep-bufferp))
        (jcommand (concat "hledger -f " 
                          (shell-quote-argument hledger-jfile)
                           " " 
                           command)))
    (with-current-buffer jbuffer
      (call-process-shell-command jcommand nil t nil)
      (if bury-bufferp
          (bury-buffer jbuffer)
        (pop-to-buffer jbuffer))
      (beginning-of-buffer))
    jbuffer))
      
(defun hledger-jreg (pattern)
  "Run hledger register command."
  (interactive "spattern> ")
  (let ((jcmd (concat "register -w 150 " pattern)))
    (hledger-jdo jcmd)
    (delete-other-windows)))

(defun hledger-compute-next-reporting-time ()
    "Computes the time we must sent the email reports. "
    (let* ((now hledger-email-next-reporting-time)
           (next-month-time (time-add now (days-to-time 30)))
           (next-month-day (string-to-number
                            (format-time-string "%d" next-month-time)))
           (delta (days-to-time (- 15 next-month-day)))
           (next-time (time-add next-month-time delta)))
      next-time))

(defun hledger-monthly-report (&optional keep-bufferp bury-bufferp)
  "Build the monthly report.
I make reports from 15th of the Month to 15th of the next month."
  (interactive)
  (let* ((now (current-time))
         (day (string-to-number (format-time-string "%d" now)))
         (end-time (time-add now (days-to-time (- 15 day))))
         (previous-time (time-subtract end-time (days-to-time 31)))
         (previous-day (string-to-number (format-time-string "%d"
                                                             previous-time)))
         (beg-time (time-add previous-time (days-to-time
                                            (- 15 previous-day))))
         (beg-time-string (format-time-string "%Y/%m/%d" beg-time))
         (end-time-string (format-time-string "%Y/%m/%d" end-time)))
    (hledger-jdo (format "balance expenses income --flat -b %s -e %s"
                         beg-time-string
                         end-time-string)
                 keep-bufferp
                 bury-bufferp)
    (with-current-buffer (get-buffer hledger-reporting-buffer-name)
      (goto-char (point-min))
      (insert (format "Report: %s - %s\n====================\n"
                      (format-time-string "%B %Y" beg-time)
                      (format-time-string "%B %Y" end-time)))
      (let ((beg (point)))
        (while (not (looking-at "--"))
          (forward-line))
        (sort-numeric-fields 2 beg (point))
        (reverse-region beg (point)))
      (forward-line 2)
      (insert "\nExpenses [Top Level]\n====================\n"))
    (hledger-jdo (format "balance expenses --depth 2 --flat -b %s -e %s"
                         beg-time-string
                         end-time-string)
                 t
                 bury-bufferp)
    (with-current-buffer (get-buffer hledger-reporting-buffer-name)
      (goto-char (point-min))
      (while (not (looking-at "Expenses"))
        (forward-line))
      (forward-line 2)
      ;; Sorting and add trailing newlines
      (let ((beg (point)))
        (while (not (looking-at "--"))
          (forward-line))
        (sort-numeric-fields 2 beg (point))
        (forward-line 2)
        (insert "\n\n")
        (forward-line -4)
        (reverse-region beg (point)))
      ;; Back to the start
      (goto-char (point-min))
      (when bury-bufferp
        (bury-buffer)))))

(defun hledger-running-report (&optional keep-bufferp bury-bufferp)
  "Show the cashflow for the past 5 months."
  (interactive)
  (let* ((beg-time (time-subtract (current-time) (days-to-time (* 4 31))))
         (beg-time-string (format-time-string "%Y/%m/%d" beg-time)))
    (hledger-jdo (format "balance expenses income --depth 2 -META -b %s"
                         beg-time-string)
                 keep-bufferp
                 bury-bufferp)
    (when (not bury-bufferp)
      ;; This is because the running report is usually very wide.
      (pop-to-buffer hledger-reporting-buffer-name)
      (delete-other-windows))
    ;; Let's sort according to the average column now
    (with-current-buffer hledger-reporting-buffer-name
      (goto-char (point-min))
      (while (not (looking-at "=="))
        (forward-line))
      (forward-line)
      (let ((beg (point)))
        (while (not (looking-at "--"))
          (forward-line))
        (sort-numeric-fields -1 beg (point))
        (reverse-region beg (point)))
      (goto-char (point-max))
      (insert "\nExpanded Running Report\n=======================\n\n"))
    (hledger-jdo (format "balance expenses income --tree -META -b %s"
                         beg-time-string)
                 t
                 bury-bufferp)))

(defun hledger-generate-reports-to-email ()
  "Generate the text html for monthly and running reports.

Returns a cons cell with (text . html).
This requires htmlfontify.el"
  (require 'htmlize)
  (hledger-running-report nil t)
  (hledger-monthly-report t t)
  (deactivate-mark t)
  (with-current-buffer hledger-reporting-buffer-name
    ;; So that no line is highlighted. The buffer is in hledger-view-mode.
    (hl-line-mode -1)
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           (htmlize-output-type 'inline-css)
           (fontified-buffer  (htmlize-buffer))
           (html (with-current-buffer fontified-buffer
                   (buffer-substring-no-properties (point-min) (point-max)))))
      (kill-buffer fontified-buffer)
      `(,text . ,html))))

(defun hledger-mail-reports ()
  "Email reports to yourself every month.
This requires utils.el which is available in utils/ alonside
hledger-mode/ directory.

Returns t if the operation was successful.
"
  (interactive)
  (require 'utils)
  (let* ((hledger-reporting-buffer-name "*Personal Finance Email*")
         (text-html-pair (hledger-generate-reports-to-email))
         (reports-text (car text-html-pair))
         (reports-html (cdr text-html-pair))
         (success (utils-send-email hledger-email-api-url
                                    (concat hledger-email-api-user ":"
                                            hledger-email-api-password)
                                    hledger-email-api-sender
                                    hledger-email-api-recipient
                                    "Monthly Financial Report"
                                    reports-text
                                    reports-html)))
    (kill-buffer hledger-reporting-buffer-name)
    (message (if success
                 "Hledger email reporting: Ok"
               "Hledger email reporting: Failed"))
    success))

(defun hledger-mail-reports-run-async-task ()
    "Async task for emailing the reports.
This isn't meant to be useful for anybody other than myself. This is extermely
inefficient."
    (require 'async)
    (async-start
     `(lambda ()
        (load-file "~/.emacs.d/init.el")
        (ignore-errors (hledger-mail-reports)))
     (lambda (success)
       (if success
           (progn
             (customize-save-variable 'hledger-email-next-reporting-time
                                      (hledger-compute-next-reporting-time))
             (message "Hledger email reporting: Ok"))
         (message "Hledger email reporting: Failed")
         (run-with-timer hledger-email-reporting-retry-interval nil
                         'hledger-mail-reports-run-async-task)))))

(defun hledger-enable-reporting ()
  "Report every month on `hledger-email-reporting-day'."
  (when (time-less-p hledger-email-next-reporting-time (current-time))
    (hledger-mail-reports-run-async-task)))

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
                                    (modify-syntax-entry ?\; "<" st)
                                    (modify-syntax-entry ?\n ">" st)
                                    st)
  "Syntax table for hledger mode.")

(defun hledger-mode-init ()
  "Function that does initial setup in the major-mode function."
  (use-local-map hledger-mode-map)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-line)
  (setq-local indent-region-function 'hledger-indent-region-function)
  (hledger-source-init)
  (setq-local ac-sources '(ac-source-hledger-source))
  (setq-local comment-start "; ")
  (setq-local comment-end "")
  (electric-indent-local-mode -1))

;;;###autoload
(define-derived-mode hledger-mode prog-mode "HLedger" ()
  "Major mode for editing hleder mode files."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (hledger-mode-init))

;;;###autoload
(define-derived-mode hledger-view-mode prog-mode "HLedger View" ()
  "Major mode for viewing hledger reports. I have a separate major mode
so that the key bindings are not shared between buffers that are used for
viewing reports and the journal file. I require the same kind of syntax
highlighting in both kinds of buffers."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (hledger-mode-init)
  ;; Highlight current line for better readability
  (hl-line-mode 1)
  ;; A freshly preprared keymap
  (use-local-map (make-keymap)))

(provide 'hledger-mode)

