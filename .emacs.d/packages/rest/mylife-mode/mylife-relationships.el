;;; Evaluate and improve your relationships.

(require 'widget)
(require 'custom)
(require 'wid-edit)

(defcustom mylife-form-file-path (expand-file-name "~/miscellany/assets/rrf.org")
  "File path of the relationship form"
  :type 'string
  :group 'mylife)

(defcustom mylife-form-buffer-name "*Relationship Rating Form*"
  "Name of the buffer displaying the form."
  :type 'string
  :group 'mylife)

(defvar mylife-form-widget-tree nil
  "A tree of wiget objects.")

;;; Reader 
(defun mylife-form-reader (form-file-path)
    "Returns an elisp representation of the form questions.
Currently I use org-mode's parser api to get a tree representation of 
the form. This function expectes `form-file-path` to be an org file."
  (with-temp-buffer 
    (insert-file-contents mylife-form-file-path)
    (org-mode)
    (goto-char 0)
    (let ((form-tree (org-element-parse-buffer)))
      form-tree)))

(defvar mylife-options
  '(("Not at all" . 1)
    ("Very little" . 2)
    ("Slightly (Or rarely)" . 3)
    ("Somewhat (not often)" . 4)
    ("A fair amount" . 5)
    ("Very much" . 6)
    ("A great deal" . 7)
    ("Strongly (almost always)" . 8)
    ("Completely or extremely" . 9)))

;;; View
(defun mylife-widget-score (&optional parent)
  "Create a widget for keeping score. The :parent-score-wid for the root
is nil."
  (let ((wid (widget-create 'number
                            :tag "Score"
                            :size 1
                            :value 5)))
    (widget-put wid :parent-score-wid parent)
    wid))


(defun mylife-widget-form (object)
  "Create a widget for the whole form."
  (let* ((heading "Relationship Rating Form")
         (top-level-objects (org-element-map object 'headline
                              'identity
                              nil nil
                              'headline))
         (total-score-wid (mylife-widget-score nil))
         (children
          (mapcar (lambda (top-level-object)
                    (mylife-widget-top-level top-level-object total-score-wid))
                  top-level-objects)))
    (widget-put total-score-wid :child-score-wids children)
    (setq header-line-format heading)
    (goto-char 0)))

(defun mylife-widget-top-level (object &optional parent)
  "Create a widget for a top level category."
  (let* ((heading (org-element-property :raw-value object))
        (contents (org-element-contents object))
        (sub-categories (org-element-map  contents
                            'headline
                          'identity))
        (nil-id (widget-insert (format "\n%s " heading)))
        (wid (mylife-widget-score parent))
        (children (mapcar (lambda (sub-category)
                            (mylife-widget-sub-category sub-category wid))
                          sub-categories)))
    (widget-put wid :child-score-wids children)
    wid))
    

(defun mylife-widget-sub-category (object &optional parent)
  "Create a widget for a sub category under the top level category."
  (let* ((heading (org-element-property :raw-value object))
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
  (let* ((bullet (org-element-property :bullet object))
         (paragraph (car (org-element-contents object)))
         (question (car (org-element-contents paragraph))))
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
           :value 5
           :parent-score-wid parent
           :notify (lambda (wid &rest ignore)
                     (message (format "You selected %d" (widget-value wid)))
                     (while (and wid (widget-get wid :parent-score-wid))
                       (let* ((parent-wid (widget-get wid :parent-score-wid))
                              (children (widget-get parent-wid :child-score-wids))
                              (child-scores (mapcar 'widget-value children))
                              (child-count (+ (length children) 0.0))
                              (average-score (/ (apply '+ child-scores) child-count)))
                         (widget-value-set parent-wid average-score)
                         (setq wid parent-wid)))
                     (widget-setup))
           (mapcar (lambda (option)
                     `(item :tag ,(car option)
                            :value ,(cdr option)))
                   options))))

              
;;; Create widgets using the above functions
(defun mylife-form-create-widget (form-object)
    "Creates the widgets given a the form object"
    (with-current-buffer (pop-to-buffer
                          (get-buffer-create mylife-form-buffer-name))
      (kill-all-local-variables)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (remove-overlays)
      (let ((local-widget-keymap (make-sparse-keymap)))
        (set-keymap-parent local-widget-keymap widget-keymap)
        (use-local-map local-widget-keymap))
      (mylife-widget-form form-object)
      (widget-setup)))

(defun  mylife-relationship-form-display ()
  "Displays the form widget."
  (interactive)
  (let ((form-object (mylife-form-reader mylife-form-file-path)))
    (mylife-form-create-widget form-object)))

(provide 'mylife-relationships)
