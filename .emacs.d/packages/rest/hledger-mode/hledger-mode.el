;; A mode for writing hledger journal entries

(defvar hledger-mode-hook nil)

;; An almost empty key map
(defvar hledger-mode-map (make-keymap))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.journal\\'" . hledger-mode))

(defconst hledger-font-lock-keywords-1
  (list
   '("\\<\\(assets\\|liabilities\\|equity\\|expenses\\|income\\)\\>" . font-lock-variable-name-face)
   '("\\<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\>" . font-lock-string-face)
   '("\\<₹ [0-9]+\\(\\.[0-9]+\\)?\\>" . font-lock-constant-face)
   )
  "Minimal highlighting expressions for hledger mode")

(defvar hledger-font-lock-keywords hledger-font-lock-keywords-1
  "Default highlighting expressions for hledger mode")

(defvar hledger-mode-syntax-table (make-syntax-table))

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

(provide 'hledger-mode)
