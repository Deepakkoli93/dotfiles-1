;; A mode for writing hledger journal entries with some convenient functions
;; Author: Narendra Joshi

(defcustom hledger-mode-hook nil
  "Normal hook for entering hledger mode."
  :type 'hook)

;; An almost empty key map
(defvar hledger-mode-map
  (let ((map (make-keymap)))
    (define-key map [backtab] '(lambda ()
                                 (backward-delete-char-untabify default-tab-width)))
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
                                    st))

;; Functions written for my convenience
(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance" "register"
                                 "incomestatement" "balancesheet" "cashflow" "activity"
                                 "stats")
  "A collection of commands that can be passed to `hledger-jdo` function defined below.")
(defvar hledger-jfile "~/miscellany/personal/finance/accounting.journal"
  "Location of the journal file.")

(defun hledger-jentry ()
  "Make a new entry in the financial journal."
  (interactive)
  (progn
    (find-file hledger-jfile)
    (goto-char (point-max))
    (yas-insert-snippet "jentry")
    (recenter)))

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
  (interactive "Mpattern> ")
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
  (setq-local indent-line-function 'hledger-indent-line))

(provide 'hledger-mode)
