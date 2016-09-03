;;; hledger-reports.el --- Generating reports with hledger  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Narendra Joshi

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; Keywords: convenience, local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains functions that are called everywhere else to
;; generate reports using hledger binary.

;;; Code:

(require 'hledger-core)

(defconst hledger-jcompletions '("print" "accounts" "balancesheet" "balance"
                                 "register" "incomestatement" "balancesheet"
                                 "cashflow" "activity" "stats"
                                 "monthly-report"
                                 "running-report"
                                 "overall-report")
  "Commands that can be passed to `hledger-jdo` function defined below.")

(defcustom hledger-ratios-liquid-asset-accounts
  "assets:bank assets:wallet"
  "Account names [separated by spaces] that contain your liquid assets"
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-nondiscritionary-expense-accounts
  "expenses:housing expenses:eating expenses:family"
  "Account names [separated by spaces] that contain non-disctrionary expenses"
  :group 'hledger
  :type 'string)

(defcustom hledger-ratios-debt-accounts
  "liabilities"
  "Account names [separated by spaces] that are liabilities."
  :group 'hledger
  :type 'string)

(defun format-time (time)
  "Format time in \"%Y-%m-%d\" "
  (format-time-string "%Y-%m-%d" time))

(defun nth-of-mth-month (n m)
  "Returns the nth of the mth month. Current month is the zeroth."
  (let* ((time (time-add (current-time)
                         (days-to-time (* 30 m))))
         (day (string-to-number (format-time-string "%d" time)))
         (delta-time (days-to-time (- n
                                      day))))
    (time-add time delta-time)))

(defun nth-of-this-month (n)
  "Returns the time value for the nth day of the current month"
  (nth-of-mth-month n 0))

(defun nth-of-prev-month (n)
  "Returns the nth day's time for the previous month."
  (nth-of-mth-month n -1))

(defun hledger-shell-command-to-string (command-string)
  (shell-command-to-string (concat "hledger -f "
                                   (shell-quote-argument hledger-jfile)
                                   " "
                                   command-string)))

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

(defun hledger-get-perfin-buffer (&optional keep-bufferp fetched-entriesp)
  "Get/create the `hledger-reporting-buffer-name' buffer.
If the buffer is not intended for editing, then `q` closes it.
`C-c y` copies the whole buffer to clipboard. "
  (let ((jbuffer (get-buffer-create hledger-reporting-buffer-name)))
    (with-current-buffer jbuffer
      (if fetched-entriesp
          (progn
            (hledger-mode)
                                   ;; Hard-coding bindings. I have anonymous functions.
            (setq header-line-format "C-c i : Append to journal"))
        (hledger-view-mode)
                                   ;; Hard-coding bindings. Need to have named functions and use `where-is'.
        (setq header-line-format "C-c q : Quit | C-c w : Copy to clipboard "))
      (or keep-bufferp (erase-buffer)))
    jbuffer))

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
    (`"overall-report" (hledger-overall-report)
     (pop-to-buffer hledger-reporting-buffer-name)
     (delete-other-windows))
    (_ (hledger-jdo command)))
  (with-current-buffer hledger-reporting-buffer-name
    (view-mode 1)))

(defun hledger-get-accounts ()
  "Returns list of account names"
  (let* ((hledger-jfile (buffer-file-name))
         (accounts-string (shell-command-to-string
                           (concat "hledger -f" hledger-jfile " accounts")))
         (accounts-list (split-string accounts-string)))
    accounts-list))

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
      (goto-char (point-min)))
    jbuffer))
      
(defun hledger-jreg (pattern)
  "Run hledger register command."
  (interactive "spattern> ")
  (let ((jcmd (concat "register -w 150 " pattern)))
    (hledger-jdo jcmd)
    (delete-other-windows)))

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
  "Show the balance report for the past 5 months."
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

(defun hledger-generate-ratios ()
  "Computes various personal finance ratios:

Computes the emergency fund ratio for the current month.
EFR = (Current liquid assets)/(Monthly nondiscretionary expenses)

I consider expenses on housing, eating and family to be
non-discretionary. Shoot for keeping it 6. Too high isn't
efficient. Too low isn't safe.

Computes the current ratio which gives you an estimate of how your current
asset vs liability situation is. Current ratio = assets / liabilities

Debt ratio = liabilities / assets

Returns a plist of the ratios.

Note: Currently this is extermely inefficient. It spawns hledger
three times.

"
  (interactive)
  (let* ((assets-report-output
          (hledger-shell-command-to-string (concat 
                                            " balance "
                                            hledger-ratios-liquid-asset-accounts
                                            " --depth 1")))
         (assets (string-to-number (nth 1 (split-string assets-report-output))))
         (expenses-report-output
          (hledger-shell-command-to-string
           (concat " balance "
                   hledger-ratios-nondiscritionary-expense-accounts
                   " --depth 1 "
                   " --begin " (format-time (nth-of-mth-month 15 -12))
                   " --end " (format-time (nth-of-this-month 15))
                   )))
         (expenses (/ (string-to-number (nth 1 (split-string expenses-report-output)))
                      12))
         (liabilities-report-output
          (hledger-shell-command-to-string (concat " balance "
                                                   " --depth 1 "
                                                   hledger-ratios-debt-accounts)))
         (liabilities (- (string-to-number (nth 1 (split-string liabilities-report-output))))))
    (list 'efr (/ assets (* expenses 1.0))      ;; Emergency fund ratio
          'cr  (/ assets (* liabilities 1.0))   ;; Current ratio
          'dr (/ liabilities (* assets 1.0))))) ;; Debt ratio

(defun current-ratio ()
  "Computes the ratio of our current liabilities to our current assets.")

(defun hledger-overall-report ()
  "A combination of all the relevant reports."
  (interactive)
  (hledger-running-report nil t)
  (hledger-monthly-report t t)
  (with-current-buffer (hledger-get-perfin-buffer t)
    (let* ((ratios (hledger-generate-ratios))
           (efr (plist-get ratios 'efr))
           (cr (plist-get ratios 'cr))
           (dr (plist-get ratios 'dr)))
      (goto-char (point-min))
      (forward-line 2)
      (insert (format (concat "\nEmergency Fund Ratio: %.2f"
                              "\nCurrent Ratio: %.2f"
                              "\nDebt Ratio: %.2f"
                              "\n\n")
                      efr cr dr)))
    (goto-char (point-min))))

(provide 'hledger-reports)
;;; hledger-reports.el ends here
