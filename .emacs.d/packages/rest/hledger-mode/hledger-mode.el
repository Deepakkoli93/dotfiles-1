;; A mode for writing hledger journal entries with some convenient functions
;; Author: Narendra Joshi

(defcustom hledger-mode-hook nil
  "Normal hook for entering hledger mode."
  :type 'hook)

;; An almost empty key map
(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map [backtab] (lambda ()
                                (interactive)
                                (backward-delete-char-untabify tab-width)))
    map))

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(defconst hledger-font-lock-keywords-1
  (list
   '("\\<\\(\\(assets\\|liabilities\\|equity\\|expenses\\|income\\|zadjustments\\)\\(:\[a-z--]+\\)*\\)\\>" . font-lock-variable-name-face)
   '("\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\>" . font-lock-string-face)
   '("\\<₹ [-]?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face))
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-defaults '(hledger-font-lock-keywords-1)
  "Default highlighting expressions for hledger mode")

(defvar hledger-mode-syntax-table (let ((st (make-syntax-table)))
                                    (modify-syntax-entry ?: "_" st)
                                    (modify-syntax-entry ?; "<" st)
                                    (modify-syntax-entry ?\n ">" st)
                                    st)
  "Syntax table for hledger mode.")

;; Functions written for my convenience
(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance"
                                 "register" "incomestatement" "balancesheet"
                                 "cashflow" "activity" "stats")
  "Commands that can be passed to `hledger-jdo` function defined below.")
(defvar hledger-jfile "~/miscellany/personal/finance/accounting.journal"
  "Location of the journal file.")

;; A few internal definitions
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

;; Indentation
(defvar hledger-comments-column 11
  "Column number where the comments start.")

;;;;;;;;;;;;;;;;;;;;;;;;
;; CLEANER CODE start ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(defvar hledger-empty-regex "^\\s-*$"
  "Regular expression for an empty line.")

(defvar hledger-date-regex "\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\>"
  "Regular expression for dates.")

(defvar hledger-date-and-desc-regex "\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\s-*[^[:space:]]+\\>"
  "Regular expression for matching a starting entry with some description.")

(defvar hledger-account-regex "\\<\\(\\(assets\\|liabilities\\|equity\\|expenses\\|income\\|zadjustments\\)\\(:\[a-z--]+\\)*\\)\\>"
  "Regular expression for a potential journal account.")

(defvar hledger-comment-regex "^[ \t]*;[ \t]*[^;]+"
  "Regular expression for a comment in journal file.")

(defun hledger-line-matchesp (re offset)
  "Returns a boolean value stating whether the line OFFSET 
lines above the current line starts with the regular experssion
RE."
  (save-excursion
    (forward-line offset)
    (beginning-of-line)
    (looking-at re)))

(defun hledger-cur-line-matchesp (re)
  "Returns true if current line starts with RE."
  (hledger-line-matchesp re 0))

(defun hledger-prev-line-matchesp (re)
  "Returns true if previous line matches RE."
  (hledger-line-matchesp re -1))

(defun hledger-first-loep ()
  "Returns true if the current line is the first line of a journal entry."
  (or (bobp) 
      (hledger-cur-line-matchesp hledger-date-regex)
      (hledger-prev-line-matchesp hledger-empty-regex)))

(defun hledger-second-loep ()
  "Retuns true if the current line is in the comments section of the entry."
  (or (hledger-prev-line-matchesp hledger-date-regex)
      (hledger-prev-line-matchesp hledger-comment-regex)))

(defun hledger-rest-loep ()
  "Returns true if we are writing the accounts section of the entry."
  (hledger-cur-line-matches-p hledger-account-regex))

(defun hledger-indent-first-loe ()
  "Indentation when on the first line of an entry."
  (let ((savep (point)))
    (cond
     ((hledger-cur-line-matchesp hledger-empty-regex)
      (beginning-of-line)
      (insert (format-time-string "%Y-%m-%d ")))
     ((hledger-cur-line-matchesp hledger-date-and-desc-regex)
      (ignore))
     ((hledger-cur-line-matchesp hledger-date-regex)
      (delete-region (line-beginning-position) (line-end-position))))))

  
(defun hledger-indent-line-better ()
  "Indent the current line."
  (cond
   ((hleger-first-loep) (hledger-indent-first-loe))
   ((hledger-second-loep) (hledger-indent-second-loe))
   ((hledger-rest-loep) (hledger-indent-rest-loe))
   (t (message "I wasn't expecting this! Fix me!"))))

  





;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; CLEANER CODE END ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;







(defun hledger-indent-line ()
  "Indent current line in hledger-mode buffer."
  (if (bobp)
      (indent-line-to 0)
    (let ((beg-regexp "\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\>")
          (comment-marker-regexp "^[ \t]*;[ \t]*[^;]+")
          (empty-line-regexp "^\\s-*$")
          cur-indent
          (saw-comment-p nil)
          (insert-comment-p nil))
      (save-excursion
        (beginning-of-line)
        (if (looking-at beg-regexp)
            (setq cur-indent 0)
          (progn
            (forward-line -1)
            (cond ((looking-at beg-regexp)
                   (setf cur-indent hledger-comments-column
			 insert-comment-p t))
                  ((looking-at comment-marker-regexp)
                   (setf cur-indent (current-indentation)
                         saw-comment-p t))
                  ((looking-at empty-line-regexp)
                   (setq cur-indent 0))
                  (t (setq cur-indent tab-width))))))
      (beginning-of-line)
      (if (and (or saw-comment-p insert-comment-p)
               (not (looking-at comment-marker-regexp))
               (looking-at empty-line-regexp))
          (progn
            (indent-line-to cur-indent)
            (insert-before-markers "; "))
        (indent-line-to cur-indent)))))

;;;###autoload
(define-derived-mode hledger-mode lisp-mode "HLedger" ()
  "Major mode for editing hleder mode files."
  :syntax-table hledger-mode-syntax-table
  (interactive)
  (use-local-map hledger-mode-map)
  (setq-local font-lock-defaults hledger-font-lock-defaults)
  (setq-local indent-line-function 'hledger-indent-first-loe))

(provide 'hledger-mode)
