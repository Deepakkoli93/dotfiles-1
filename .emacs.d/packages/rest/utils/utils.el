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

;;;###autoload 
(define-minor-mode utils-easy-move-mode
  "A mode for bindings in a read only environment. Mimicks vim."
  nil
  "Easy"
  (utils-easy-move (make-sparse-keymap)))

(provide 'utils)
