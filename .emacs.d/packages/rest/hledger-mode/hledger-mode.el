;; A mode for writing hledger journal entries with some convenient functions
;; Author: Narendra Joshi

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
(defvar hledger-amount-regex "\\<₹ [-]?[0-9]+\\(\\.[0-9]+\\)?\\>"
  "Regular expression to match an inserted amount in rupees.")

;; Functions written for convenice
(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance"
                                 "register" "incomestatement" "balancesheet"
                                 "cashflow" "activity" "stats")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defvar hledger--empty-line "^\\s-*$"
  "Regular expression for matching line that delimits journal entries.")
(defun hledger--go-to-starting-line ()
  "Function to go the first line that stars a new entry."
  (goto-char (point-max))
  (beginning-of-line)                     ; Go to the first non-empty
  (while (looking-at hledger--empty-line) ; line And then
    (forward-line -1))                    ; insert an empty line
  (end-of-line)                           ; and then we may expand the snippet
  (insert "\n\n"))
(defun hledger--overlay-current-entry ()
  (interactive)
  (while (and (not (bobp))
              (not (looking-at hledger--empty-line)))
    (forward-line -1))
  (forward-line 1)
  (setq begin (point))
  (forward-line 1)
  (while (and (not (looking-at hledger--empty-line))
              (not (eobp)))
    (forward-line 1))
  (setq end (point))
  (setq current-entry (make-overlay begin end))
  (overlay-put current-entry 'face '(:background "black")))

(defun hledger--clear-undo-list ()
  "Empty `buffer-undo-list`."
  (buffer-disable-undo)     
  (buffer-enable-undo))     

(defun hledger-jentry ()
  "Make a new entry in the financial journal. Avoids editing old entries."
  (interactive)
  (find-file hledger-jfile)
  (hledger--go-to-starting-line)
  (yas-insert-snippet "jentry")
  (recenter))

(defun hledger-jdo (command)
  "Run a hledger command on the journal file."
  (interactive (list (completing-read "jdo> " hledger-jcompletions)))
  (let ((jbuffer (get-buffer-create "*Personal Finance*"))
        (jcommand (concat "hledger -f " hledger-jfile " " command)))
    (with-current-buffer jbuffer
      (local-set-key (kbd "q")
                     '(lambda ()
                        (interactive)
                        (quit-restore-window (selected-window) 'kill)))
      (erase-buffer)
      (call-process-shell-command jcommand nil t nil)
      (pop-to-buffer jbuffer))))

(defun hledger-jreg (pattern)
  "Run hledger register command."
  (interactive "pattern> ")
  (let ((jcmd (concat "register " pattern)))
    (hledger-jdo jcmd)))

;; Indentation
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
(defun hledger-insert-amount ()
  "Insert the amount for a transaction in hledger"
  (beginning-of-line)
  (re-search-forward hledger-whitespace-account-regex (line-end-position) t)
  (insert "    ₹ "))
(defun hledger-acc-line-has-amountp ()
  "Returns true if the account line has an amount."
  (hledger-cur-line-matchesp (concat hledger-whitespace-account-regex
                                     "\\s-*₹")))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 ;; FIX THIS. USE SOMETHING LIKE A MACRO ;;
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hledger-cur-line-emptyp ()
  "Returns true if current line is empty."
  (hledger-cur-line-matchesp hledger-empty-regex))
(defun hledger-cur-has-datep ()
  "Returns true if current line has only date."
  (hledger-cur-line-matchesp hledger-date-only-regex))
(defun hledger-cur-has-commentp ()
  "Returns true if current line has an empty comment. Empty comments."
  (hledger-cur-line-matchesp hledger-empty-comment-regex))
(defun hledger-cur-has-accp ()
  "Returns true if the current line has an account name."
  (hledger-cur-line-matchesp hledger-whitespace-account-regex))

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
  
;;;;;;;;;;;;;;;;;;;;;;;;;
;; END OF FIX ME BLOCK ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (hledger-delete-cur-line)
  (indent-line-to tab-width))

(defun hledger-indent-account-line ()
  "Called when the line to indent is an account listing line."
  (if (hledger-acc-line-has-amountp)
      (progn (end-of-line)
             (delete-region (re-search-backward "\\s-*₹"
                                                (line-beginning-position)
                                                t)
                            (line-end-position)))
    (hledger-insert-amount)))

(defun hledger-indent-line ()
  "Indent the current line."
  (cond
   ((hledger-cur-line-emptyp) (hledger-indent-empty-line))
   ((hledger-cur-has-datep) (hledger-indent-date-line))
   ((hledger-cur-has-commentp) (hledger-indent-comment-line))
   ((hledger-cur-has-accp) (hledger-indent-account-line))))
   

;; Auto-complete
(defun hledger-source-init ()
  "Initialize the candidates list for account completion."
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

(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map [backtab] (lambda ()
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

;;;###autoload
(define-derived-mode hledger-mode prog-mode "HLedger" ()
  "Major mode for editing hleder mode files."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (use-local-map hledger-mode-map)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-line))

(provide 'hledger-mode)
