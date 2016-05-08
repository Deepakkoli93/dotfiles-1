;; -*- lexical-binding: t -*-
;;; Evaluate and improve your relationships.

(require 'widget)
(require 'wid-browse)
(require 'wid-edit)

(defcustom mylife-form-file-path (expand-file-name "~/miscellany/assets/rrf.org")
  "File path of the relationship form"
  :type 'string
  :group 'mylife)

(defcustom mylife-form-state-file (expand-file-name "~/miscellany/assets/.rrf-state.el")
  "We store state as a elisp data structure."
  :type 'string
  :group 'mylife)

(defcustom mylife-form-buffer-name "*Relationship Rating Form*"
  "Name of the buffer displaying the form."
  :type 'string
  :group 'mylife)

(defcustom mylife-header-face 'info-title-1
  "Face for the header line."
  :type 'face
  :group 'mylife)

(defcustom mylife-score-text-face 'warning
  "Face for the score text."
  :type 'face
  :group 'mylife)

(defcustom mylife-score-value-face 'default
  "Face for the score value. #TODO"
  :type 'face
  :group 'mylife)

(defcustom mylife-bullet-face 'match
  "Face for bullets"
  :type 'face
  :group 'mylife)

(defcustom mylife-option-face 'custom-active1
  "Face for the options for a question."
  :type 'face
  :group 'mylife)

(defcustom mylife-question-face 'font-lock-function-name-face
  "Face for a question."
  :type 'face
  :group 'mylife)

(defcustom mylife-top-category-face 'region
  "Face for a global scale."
  :type 'face
  :group 'mylife)

(defcustom mylife-sub-category-face 'secondary-selection
  "Face for a sub category text."
  :type 'face
  :group 'mylife)


(setq mylife-form-root-wid nil)

;;; Reader 
(defun mylife-form-reader (form-file-path)
    "Returns an elisp representation of the form questions.
Currently I use org-mode's parser api to get a tree representation of 
the form. This function expectes `form-file-path` to be an org file."
  (with-temp-buffer 
    (insert-file-contents mylife-form-file-path)
    (org-mode)
    (goto-char (point-min))
    (let ((form-tree (org-element-parse-buffer)))
      form-tree)))

(defvar mylife-options
  (let ((options '(("Not at all" . 1)
                   ("Very little" . 2)
                   ("Slightly (Or rarely)" . 3)
                   ("Somewhat (not often)" . 4)
                   ("A fair amount" . 5)
                   ("Very much" . 6)
                   ("A great deal" . 7)
                   ("Strongly (almost always)" . 8)
                   ("Completely or extremely" . 9))))
    (mapcar (lambda (option)
              (cons (propertize (car option)
                                'font-lock-face
                                mylife-option-face)
                    (cdr option)))
	    options)))


;;; View
(defun mylife-widget-score (&optional parent)
  "Create a widget for keeping score. The :parent-score-wid for the root
is nil."
  (let ((wid (widget-create 'number
                            :tag (propertize "Score" 'font-lock-face mylife-score-text-face)
                            :size 1
                            :tab-order -1
                            :value 5)))
    (widget-put wid :parent-score-wid parent)
    wid))


(defun mylife-widget-form (object)
  "Create a widget for the whole form. Every function returns the score widget
that it creates."
  (let* ((heading (propertize "Relationship Rating Form"
                              'font-lock-face
                              mylife-header-face))
         (top-level-objects (org-element-map object 'headline
                              'identity
                              nil nil
                              'headline))
         (total-score-wid (mylife-widget-score nil))
         (nil-id (widget-insert "\n"))
         (children
          (mapcar (lambda (top-level-object)
                    (mylife-widget-top-level top-level-object total-score-wid))
                  top-level-objects)))
    (widget-put total-score-wid :child-score-wids children)
    (setq header-line-format heading)
    (goto-char (point-min))
    (widget-forward 1)
    total-score-wid))

(defun mylife-widget-top-level (object &optional parent)
  "Create a widget for a top level category."
  (let* ((heading (propertize
                   (org-element-property :raw-value object)
                   'font-lock-face
                   mylife-top-category-face))
        (contents (org-element-contents object))
        (sub-categories (org-element-map  contents
                            'headline
                          'identity))
        (nil-id (widget-insert (format "\n%s " heading)))
        (wid (mylife-widget-score parent))
        (nil-idd (widget-insert (format "\n")))
        (children (mapcar (lambda (sub-category)
                            (mylife-widget-sub-category sub-category wid))
                          sub-categories)))
    (widget-put wid :child-score-wids children)
    wid))
    

(defun mylife-widget-sub-category (object &optional parent)
  "Create a widget for a sub category under the top level category."
  (let* ((heading (propertize
                   (org-element-property :raw-value object)
                   'font-lock-face
                   mylife-sub-category-face))
        (items (org-element-map object 'item 'identity))
        (nil-id (widget-insert (format "\n%s " heading)))
        (wid (mylife-widget-score parent))
        (children (mapcar (lambda (item)
                            (mylife-widget-item item wid))
                          items)))
    (widget-put wid :child-score-wids children)
    wid))

(defun mylife-widget-item (object &optional parent)
  "Create a widget for a question item under the sub-category."
  (let* ((bullet (propertize
                  (org-element-property :bullet object)
                  'font-lock-face
                  mylife-bullet-face))
         (paragraph (car (org-element-contents object)))
         (question
          (propertize (car (org-element-contents paragraph))
                      'font-lock-face
                      mylife-question-face)))
    (widget-insert (format "\n%s" bullet))
    (mylife-widget-question question mylife-options parent)))

(defun mylife-widget-question (text options &optional parent)
  "A widget to display a question. OPTIONS is an association list
with choices and their corresponding scores."
  (message text)
  (widget-insert text)
  (let* ((negativep (string-match-p ".*(R)" text))
         (compute-score (lambda (val)
                          (if negativep
                              (- 10 val)
                            val))))
    (apply 'widget-create 
           'radio-button-choice
           :negativep negativep
           :value 5
           :parent-score-wid parent
           :notify (lambda (wid &rest ignore)
                     (save-excursion 
                       (message (format "You selected %d" (widget-value wid)))
                       (while (and wid (widget-get wid :parent-score-wid))
                         (let* ((parent-wid (widget-get wid :parent-score-wid))
                                (children (widget-get parent-wid :child-score-wids))
                                (child-scores (mapcar
                                               (lambda (child)
                                                 (if (widget-get child :negativep)
                                                     (- 10 (widget-value child))
                                                   (widget-value child)))
                                               children))
                                (child-count (+ (length children) 0.0))
                                (average-score
                                 (read
                                  (format "%.2f"
                                          (/ (apply '+ child-scores) child-count)))))
                           (widget-value-set parent-wid average-score)
                           (setq wid parent-wid)))
                       (widget-setup)))
           (mapcar (lambda (option)
                     `(item :tag ,(car option)
                            :value ,(cdr option)))
                   options))))

(defun mylife-widget-browse-at (pos)
  "(*Modified*) browse the widget under point. Not using it right now
Update the docstrings when you update the code! #WAKA#"
  (let* ((field (get-char-property pos 'field))
         (button (get-char-property pos 'button))
         (doc (get-char-property pos 'widget-doc))
         (widget (or field button doc)))
    widget))

(defvar mylife-relationship-form-keymap
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map widget-keymap)
    (define-key map "n" 'widget-forward)
    (define-key map "p" 'widget-backward)
    (mapcar (lambda (digit)
              (define-key map (format "%s" digit)
                (lambda ()
                  (interactive)
                  (widget-forward (1- digit)))))
            (number-sequence 2 9))
    map))

;;; Create widgets using the above functions
(defun mylife-form-create-widget (form-object)
    "Creates the widgets given a the form object"
    (with-current-buffer (pop-to-buffer
                          (get-buffer-create mylife-form-buffer-name))
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (use-local-map mylife-relationship-form-keymap)
      (setq mylife-form-root-wid (mylife-widget-form form-object))
      (widget-setup)
      (delete-other-windows)))

(defun mylife-save-restore-helper (root)
  "Returns question widget ids traversing down from the root."
  (let* ((top-categories (widget-get root :child-score-wids))
         (sub-categories (apply 'append
                                (mapcar (lambda (w)
                                          (widget-get w :child-score-wids))
                                        top-categories)))
         (questions (apply 'append
                           (mapcar (lambda (w)
                                     (widget-get w :child-score-wids))
                                   sub-categories))))
    questions))
  

(defun mylife-save-form ()
  "Save the current state of the form to mylife-form-state-file."
  (interactive)
  (let* ((questions (mylife-save-restore-helper mylife-form-root-wid))
         (values (mapcar 'widget-value questions)))
    (write-region (format "%s" values)
                  nil
                  mylife-form-state-file)))

(defun mylife-restore-form ()
  "Restore the form stored in mylife-form-state-file."
  (interactive)
  (let* ((questions (mylife-save-restore-helper mylife-form-root-wid))
         (values (with-temp-buffer
                   (insert-file-contents mylife-form-state-file)
                   (read (buffer-substring-no-properties (point-min)
                                                   (point-max))))))
    (mapcar* (lambda (question value)
               (widget-value-set question value)
               (widget-apply question :notify nil))
             questions
             values)))

(defun  mylife-rrf ()
  "Displays the form widget."
  (interactive)
  (let ((form-object (mylife-form-reader mylife-form-file-path)))
    (mylife-form-create-widget form-object)))

(provide 'mylife-relationships)
