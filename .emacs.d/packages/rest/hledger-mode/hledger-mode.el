;; A mode for writing hledger journal entries with some convenient functions
;; Author: Narendra Joshi

(defcustom hledger-mode-hook nil
  "Normal hook for entering hledger mode."
  :type 'hook)

;; An almost empty key map
(defvar hledger-mode-map (make-keymap))

(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(defconst hledger-font-lock-keywords-1
  (list
   '("\\<\\(\\(assets\\|liabilities\\|equity\\|expenses\\|income\\|zadjustments\\)\\(:\[a-z--]+\\)*\\)\\>" . font-lock-variable-name-face)
   '("\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\>" . font-lock-string-face)
   '("\\<₹ [-]?[0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face))
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-keywords hledger-font-lock-keywords-1
  "Default highlighting expressions for hledger mode")

(defvar hledger-mode-syntax-table (let ((st (make-syntax-table)))
                                    (modify-syntax-entry ?: "_" st)
                                    st))

;;;###autoload
(defun hledger-mode ()
  "Major mode for editing hleder mode files."
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table hledger-mode-syntax-table)
  (use-local-map hledger-mode-map)
  (set (make-local-variable 'font-lock-defaults) '(hledger-font-lock-keywords))
  (setq major-mode 'hledger-mode)
  (setq mode-name "hledger")
  (run-hooks 'hledger-mode-hook))

(defconst jcompletions '("print" "accounts" "balancesheet" "balance" "register"
                       "incomestatement" "balancesheet" "cashflow" "activity"
                       "stats")
  "A collection of commands that can be passed to `jdo` function defined below.")
(defvar jfile "~/miscellany/personal/finance/accounting.journal"
  "Location of the journal file.")

;;;###autoload
(defun jentry ()
  "Make a new entry in the financial journal."
  (interactive)
  (progn
    (find-file jfile)
    (goto-char (point-max))
    (yas-insert-snippet "jentry")
    (recenter)))

;;;###autoload
(defun jdo (command)
  "Run a hledger command on the journal file."
  (interactive (list (completing-read "jdo> " jcompletions)))
  (let ((jbuffer (get-buffer-create "*Personal Finance*"))
	(jcommand (concat "hledger -f " jfile " " command)))
    (with-current-buffer jbuffer
      (local-set-key (kbd "q")
                     '(lambda ()
                        (interactive)
                        (quit-restore-window (selected-window) 'kill)))
      (call-process-shell-command jcommand nil t nil)
      (pop-to-buffer jbuffer))))

;;;###autoload
(defun jreg (pattern)
  "Run hledger register command."
  (interactive "Mpattern> ")
  (let ((jcmd (concat "register " pattern)))
    (jdo jcmd)))

;; Auto-complete
(defun hledger-source-init ()
    "Initialize the candidates list for account completion."
  (let*
      ((accounts-string (shell-command-to-string
                         (concat "hledger -f" jfile " accounts")))
       (accounts-list (split-string accounts-string)))
    (setq hledger-source-cache accounts-list)))
(hledger-source-init)

(defvar ac-source-hledger-source
  '((init . hledger-source-init)
    (candidates . hledger-source-cache))
  "A source for completing account names in a hledger buffer.")
  
(provide 'hledger-mode)
