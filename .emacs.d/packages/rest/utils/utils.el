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

(provide 'utils)
