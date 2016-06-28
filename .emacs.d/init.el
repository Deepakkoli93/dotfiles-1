                                            ;;;;;;;;;;;;;;;;;;
                                            ;; My init.el   ;;
                                            ;;;;;;;;;;;;;;;;;;
;;; PACKAGE ARCHIVES
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-user-dir (expand-file-name "packages/elpa/"
                                         user-emacs-directory))
;; Add Customized packages to load-path
(let ((default-directory (expand-file-name "packages/rest/"
                                           user-emacs-directory)))
  (normal-top-level-add-to-load-path '("hledger-mode"
                                       "powerline"
                                       "mylife-mode")))
(package-initialize)

;;; VARIABLES
(setq secrets-file
      (expand-file-name "~/miscellany/assets/secrets.el"))
;;; secrets.el [Sets up a few variables]
(if (file-exists-p secrets-file)
    (load secrets-file))

(setq org-directory
      (expand-file-name "~/miscellany/personal/org/"))
(setq emacs-themes-directory
      (expand-file-name "~/.emacs.d/themes/"))
(setq backups-directory
      (expand-file-name "backups/"
                        user-emacs-directory))
(setq abbrev-file
      (expand-file-name "abbrev_defs"
                        user-emacs-directory))
(setq personal-dictionary-file 
      (expand-file-name "~/miscellany/assets/personal-dict.en.pws"))

;; Blog
(setq blog-dir
      (expand-file-name "~/code/blog/narendraj9.github.io"))
(setq blog-posts-dir
      (expand-file-name "web/posts/" blog-dir))
;; Hledger
(setq hledger-jfile
      (expand-file-name "~/miscellany/personal/finance/accounting.journal"))
(when (boundp 'my-hledger-service-fetch-url)
  (setq hledger-service-fetch-url
	my-hledger-service-fetch-url))

;;;APPEARANCE
(add-to-list 'custom-theme-load-path
             emacs-themes-directory)

(defun set-appearance ()
  (interactive)
  "Set up the appearance of emacs."
  (progn
    ;; Make the window simpler.
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)

    ;; Buffer background and foreground
    (setq blackboard-theme-color "#0C1021")
    (setq black-theme-color "gray9")
    (set-background-color black-theme-color)
    (set-foreground-color "white")

    ;; Pretty window divier
    (set-face-background 'vertical-border "peru")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))

    ;; Window margins | 2 pixels on each side
    (fringe-mode 2)
    (set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")
    
    ;; Powerline
    (require 'powerline)
    (require 'powerline-vermilion-theme)
    (if (window-system) (powerline-vermilion-theme))))
    
(set-appearance)

;;; GLOBAL KEY BINDINGS
(global-set-key (kbd "M-[") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'switch-to-minibuffer)
;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c k") 'kill-buffer-delete-window)
;; personal accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-jdo)
;; utilities
(global-set-key (kbd "C-c d") 'insert-date-at-point)
(global-set-key (kbd "C-c L") 'linum-mode)
(global-set-key (kbd "C-c =") 'vicarie/eval-print-last-sexp)
(global-set-key (kbd "C-c i") 'go-back-to-intellij)
(global-set-key (kbd "C-c q") 'fill-paragraph-and-move-forward)
(global-set-key (kbd "C-c u") 'enlarge-current-window)

;; rarely used bindings
(global-set-key (kbd "C-c y") 'yank-to-x-clipboard)
(global-set-key (kbd "M-x") 'helm-M-x)

;;; UTILITY FUNCTION DEFINITIONS
(defun enlarge-current-window ()
  "Enlarge the current window by 5 lines."
  (interactive)
  (enlarge-window 5))

(defun trim (s)
  "Removes trailing whitespace."
  (replace-regexp-in-string 
   "[ \t\n]+$"
   ""
   s))

(defun fill-paragraph-and-move-forward ()
    "A simple function that combines `fill-paragraph'
and `forward-paragraph' because I tend to use then together always."
  (interactive)
  (fill-paragraph)
  (forward-paragraph))

(defun erc-connect ()
  "Connect to erc."
  (interactive)
  (require 'erc-services)
  (require 'tls)
  (erc-services-mode 1)
  (if (boundp 'my-freenode-nickserv-password) 
      (erc-tls :server "irc.freenode.net"
               :port 6697
               :nick "narendraj9"
               :password my-freenode-nickserv-password)
    (message "Error: my-freenode-nickserv-password not bound")))

(defun slack-connect ()
  "Connect to slack."
  (interactive)
  (require 'erc-services)
  (require 'tls)
  (erc-services-mode 1)
  (if (boundp 'my-slack-vicarie-password)
      (erc-tls :server "vicarie.irc.slack.com"
               :port 6697
               :nick "narendraj9"
               :password my-slack-vicarie-password)
    (message "Error: my-slack-vicarie-password not bound")))

(defun vicarie/eval-print-last-sexp ()
    "Evaluate and print the last sexp on the same line."
  (interactive)
  (let* ((standard-output (current-buffer))
         (value (eval-last-sexp nil)))
    (insert-string (format " [= %s ] " value))))

(defun create-file-for-buffer ()
  "Create a temporary file for the current buffer. To be used for buffers
that don't have an associated file."
  (let* ((temp-file (make-temp-file
                     (replace-regexp-in-string "\*"
                                               ""
                                               (buffer-name))
                     nil
                     ".txt")))
    (write-region (point-min) (point-max) temp-file)
    temp-file))  

(defun upload-file (file-path)
  "Upload a file to transfer.sh using curl. I am thinking that
using curl is more efficient for binary files than using a buffer
and calling `upload-buffer'."
  (interactive "fFile: ")
  (kill-new (shell-command-to-string
             (format "%s %s %s%s"
                     "curl -s --upload-file"
                     (shell-quote-argument file-path)
                     "https://transfer.sh/"
                     (shell-quote-argument
                      (file-name-nondirectory file-path)))))
  (message (format "Link copied to clipboard: %s"
                   (trim (current-kill 0)))))

(defun upload-buffer ()
  "Upload current buffer to transfer.sh
This function uses the function `upload-region'."
  (interactive)
  (upload-region (point-min) (point-max)))

(defun upload-region (beg end)
  "Upload the contents of the selected region in current buffer
using transfer.sh Link to the uploaded file is copied to
clipboard. Creates a temp file if the buffer isn't associted with
a file."
  (interactive "r")
  (let* ((buf-file-path (buffer-file-name))
         (file-path (or buf-file-path
                        (create-file-for-buffer)))
         (file-name (file-name-nondirectory file-path))
         (upload-url (format "https://transfer.sh/%s"
                            file-name))
         (url-request-method "PUT")
         (url-request-data (buffer-substring-no-properties beg end))
         (url-callback (lambda (status)
                         (search-forward "\n\n")
                         (let ((url-link (buffer-substring (point)
                                                           (point-max))))
                           (kill-new url-link)
                           (message (format "Link copied to clipboard: %s"
                                            (trim url-link)))
                         (kill-buffer (current-buffer))))))
    (url-retrieve upload-url url-callback)))
    
(defun org-late-todo (n)
  "Switch todo assuming an old date [n days ago]"
  (interactive "nDays: ")
  (let* ((delta-time (days-to-time n))
         (now (time-subtract (current-time)
                             delta-time)))
    (letf (((symbol-function 'current-time) (lambda () now)))
      (org-agenda-todo))))
    
(defun switch-to-minibuffer ()
  "Switch to minibuffer."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (active-minibuffer-window))))

(defconst iso-time-format "%FT%T%z"
  "Full ISO 8601 time format")

(defun read-date (&optional format)
  "Get date from the user and return it in the format FORMAT. 
If format isn't specified it defaults to `%Y %m %d`"
  (format-time-string (if format format "%Y %m %d")
                      (org-time-string-to-time (org-read-date))))

(defun make-old-content-read-only ()
  "Only allow for appending new content in the buffer."
  (interactive)
  (put-text-property 1 2 'front-sticky '(read-only))
  (save-excursion
    (let ((begin (point-min))
          (end (progn
                 (end-of-buffer)
                 (backward-word)  ; Upto the line containing a word
                 (end-of-line nil)
                 (point))))
      (put-text-property begin end 'read-only t))))

;; Minor mode for enabling rainbow mode everywhere
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (memq major-mode '(eshell-mode org-agenda-mode erc-mode Info-mode)))
      (rainbow-mode))))

(defun kill-buffer-delete-window ()
  "Kill current buffer and delete its window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun cleanup-whitespace ()
  "Remove whitespaces. "
  (interactive)
  (whitespace-cleanup)
  (delete-trailing-whitespace)
  (message "Cleaned up whitespaces!"))

(defun inhibit-read-only ()
  "Avoid read-only mode. 
Because eshell is silly and into read-only mode on typing over prompt."
  (interactive)
  (setq inhibit-read-only t))

(defun kill-with-linenum (beg end)
  "Kill region with the line numbers."
  (interactive "r")
  (save-excursion
    (goto-char end)
    (skip-chars-backward "\n \t")
    (setq end (point))
    (let* ((chunk (buffer-substring beg end))
           (chunk (concat
                   (format "╭──────── #%-d ─ %s ──\n│ "
                           (line-number-at-pos beg)
                           (or (buffer-file-name) (buffer-name))
                           )
                   (replace-regexp-in-string "\n" "\n│ " chunk)
                   (format "\n╰──────── #%-d ─"
                           (line-number-at-pos end)))))
      (kill-new chunk)))
  (deactivate-mark))

(defun he-toggle ()
  "Toggle highlight expression inside selected parens. 
Useful when showing code."
  (interactive)
  (if (equal show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-paren-style 'parenthesis)))

(defun kill-other-buffers ()
  "Kill all buffers except the current one and the erc buffers."
  (interactive)
  (let ((ignore-buffers (cons (current-buffer)
                              (if (fboundp 'erc-buffer-list)
                                  (erc-buffer-list)
                                nil))))
    (mapc (lambda (buffer)
            (when (not (memq buffer ignore-buffers))
              (kill-buffer buffer)))
          (buffer-list))))

(defun yank-to-x-clipboard (&optional region-beg region-end)
  "Yank selected text to X clipboard. Use when on console."
  (interactive "r")
  (shell-command-on-region region-beg region-end "xclip -i -selec clip"))

(defun easy-move ()
  "Set key-binding for easier navigation. Use when in read-only/view-mode."
  (interactive)
  (local-set-key (kbd "h") 'backward-char)
  (local-set-key (kbd "l") 'forward-char)
  (local-set-key (kbd "j") 'next-line)
  (local-set-key (kbd "k") 'previous-line))

(defun uneasy-move ()
  "Reset key bindings set by easy-move."
  (interactive)
  (local-unset-key (kbd "h"))
  (local-unset-key (kbd "l"))
  (local-unset-key (kbd "j"))
  (local-unset-key (kbd "k")))

(defun blog-post (file)
  "Start a post in the blog-dir directory"
  (interactive (list (let ((default-directory blog-posts-dir))
                       (read-file-name "Filename: "))))
  (find-file file)
  (yas-expand "post"))

(defun insert-date-at-point ()
  "Insert current date at the current position of point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

(defun notify (msg &optional font-size duration)
  "Notify me with a msg. Requires that dzen is installed."
  (start-process-shell-command "dzen" nil
                               (format "echo %s | dzen2 -l 200 -fn 'Comic Sans MS:size=%s' -p %s"
                                       (shell-quote-argument msg)
                                       (or font-size 50)
                                       (or duration 10))))

;; Setup an emacs window into 70-30% horizontally.
(fset 'split-thirty-seventy
      "\C-x2\C-u4\C-x^\C-u4\C-x^")

(defun split-and-shell ()
  "Split the buffer vertically and shart shell in one of the windows."
  (interactive)
  (execute-kbd-macro (symbol-function 'split-thirty-seventy))
  (other-window 1)
  (eshell))

(defun package-install-missing-packages ()
  "Function for installing missing packages."
  (interactive)
  (setq package-list
        '(android-mode auto-complete popup clojure-mode cmake-mode
                       coffee-mode color-theme epl epresent geiser
                       gh logito pcache ghci-completion haskell-mode
                       inf-ruby logito magit magit-popup dash async
                       git-commit with-editor dash async dash with-editor
                       dash async dash async magit-popup dash async markdown-mode
                       matlab-mode notmuch org-pomodoro alert log4e gntp pcache
                       popup powerline python-mode rainbow-delimiters rainbow-mode
                       sml-mode with-editor dash async yasnippet))
  (package-refresh-contents)
  ;; Install missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

;;; Functional Playground
(defun snap-it-to-file ()
  "Take a screenshot of emacs and return the file path."
  (make-directory "/tmp/screenshots/" t)
  (let ((default-directory "/tmp/screenshots/"))
    (shell-command-to-string
     "scrot -u -e 'echo -n /tmp/screenshots/$f'")))

(defun snap-it ()
  "Take a screenshot and upload it to transfer.sh"
  (interactive)
  (upload-file (snap-it-to-file)))

(defun go-back-to-intellij ()
  "Change focus to window running android studio."
  (interactive)
  (shell-command "wmctrl -a 'Android Studio'"))
 
(defun post-to-slack (webhook-url text)
  "Post text to the slack webhook-url"
  (let ((url-request-method "POST")
        (url-request-data (json-encode `(:text ,text)))
        (url-request-extra-header '(("Content-Type" . "application/json")))
        (url-callback  (lambda (status)
                         (search-forward "\n\n")
                         (let ((info-text (buffer-substring (point)
                                                            (point-max))))
                           (message (format "slack: %s"
                                            (trim info-text)))
                           (kill-buffer (current-buffer))))))
    (url-retrieve webhook-url url-callback)))

(defun post-region-to-slack-cooking (beg end)
  "Post region to one of my slack channels."
  (interactive "r")
  (if (boundp 'my-slack-vicarie-cooking-webhook)
      (post-to-slack my-slack-vicarie-cooking-webhook
                     (buffer-substring beg end))
    (message "`my-slack-vicarie-cooking-webhook` not bound to the webhook url")))

;;; MISCELLANY
;; Keep a separate custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file))

;; personal finance
(require 'hledger-mode)
(add-hook 'hledger-mode-hook 'easy-auto-complete-mode-hook)
(add-hook 'hledger-mode-hook (lambda ()
                               (flyspell-mode 1)))

;; mylife-mode
(require 'mylife-mode)

;; get quick emacs key binding suggestions
(require 'guide-key)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4"))
(guide-key-mode 1) 

;; ido
;; show completions vertically
(setq ido-decorations (quote
                       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                        " [Matched]" " [Not readable]" " [Too big]"
                        " [Confirm]")))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
;; Ignore listing irc buffers and helm session buffers.
(setq ido-ignore-buffers '("\\` "  "^#.*" ".*freenode\.net.*" "\\*helm.*"))
(ido-mode t)

;; An alternative to ido. Maybe addictive but is it good?
(require 'helm-config)
;; To ignore warnings about redefinitions
(setq ad-redefinition-action 'accept)
(helm-mode 1)

;; line numbers for rows
(global-linum-mode 0)
(setq linum-format "%2d| ")
(set-face-attribute 'linum nil
                    :background "black"
                    :foreground "steel blue")

;; tramp-mode
(setq tramp-default-method "ssh")
;; make backups for tramp files in their original locations
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

;; spell-checking
(dolist (hook '(markdown-mode-hook latex-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1))))
(setq ispell-personal-dictionary personal-dictionary-file)
                                 

;; easy-move bindings in man-mode as well
(add-hook 'Man-mode-hook 'easy-move)
;; width of man pages
(setenv "MANWIDTH" "80")
;; frame size and position on emacs startup | for consitency
(set-frame-size (selected-frame) 100 30)
(set-frame-position (selected-frame) 250 150)

;; cmake-mode
(require 'cmake-mode)

;; whitespace-mode | For the 80-column rule
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-global-modes '(not erc-mode eshell-mode org-agenda-mode Info-mode))
(global-whitespace-mode 1)

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
(yas-global-mode 1)

;; Auto-complete stuff
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-ignore-case nil)
(ac-flyspell-workaround)

;; Start these modes with auto-complete on
(add-to-list 'ac-modes 'hledger-mode)

;; Usually, do not complete automatically
;; Auto-complete trigger on Meta-Tab
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)

;; Inline completion suggestions are distracting!
(setq ac-disable-inline t)

;; Make auto-complete easier for a mode
(defun easy-auto-complete-mode-hook ()
  (setq-local ac-auto-start t)
  (setq-local ac-auto-show-menu 0.4)
  (setq-local ac-disable-inline nil))

;; Key bindings for auto-complete-menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
(define-key ac-menu-map (kbd "TAB") 'ac-complete)

;; Unique buffer names:
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " • ")

;; recent files menu | remote files mess things up
(add-hook 'recentf-dialog-mode-hook 'easy-move)
(setq recentf-auto-cleanup 'never)
(setq recentf-kepp '(file-remote-p file-readable-p))
(recentf-mode 1)

;; save all backup files in a fixed directory
(setq auto-save-list-file-prefix
      (concat backups-directory "/autosaves-"))
(setq backup-directory-alist
      `((".*" . ,backups-directory)
        backup-by-copying t
        version-control t
        delete-old-versions t
        kept-new-versions 5
        kept-old-versions 5))
(setq auto-save-file-name-transforms
      `((".*" ,backups-directory t)))

;; abbrev-mode
(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(setq abbrev-file-name abbrev-file)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; general settings
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

(setq column-number-mode t)
(setq-default tab-width 4)

;; miscellany (maybe)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-splash-screen t)

;;; FONTS
;; Fallback font for characters not available in Monaco
(set-fontset-font "fontset-default" nil
                  (font-spec :size 20 :name "Symbola"))

;;; ESHELL
(defmacro with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))
;; Ignore consecutive duplicates in eshell history
(setq eshell-hist-ignoredups t)
;; Don't cycle through possible completions
(setq eshell-cmpl-cycle-completions nil)
;; My eshell-prompt
(setq eshell-highlight-prompt nil)
(setq eshell-prompt-function
      (lambda ()
        (concat
         (with-face "(" :foreground "white")
         (with-face (file-name-nondirectory (eshell/pwd)) :foreground "tomato")
         (with-face ")" :foreground "white")
         (with-face (if (= (user-uid) 0) " #" " λ") :foreground "pale green" :bold)
         (with-face " " :foreground "white"))))
(setq eshell-prompt-regexp "^[^#λ\n]* [#λ] ")
;; Set up visual comamnds and sub-commands
(add-hook 'eshell-mode-hook '(lambda ()
                               (add-to-list 'eshell-visual-commands "vim")
                               (add-to-list 'eshell-visual-subcommands
                                            '("git" "commit" "log" "diff"))))
;;; WRITING
(setq fill-column 79)

;;; HASKELL-MODE
;; (load "haskell-mode-autoloads")
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'haskell-indentation-mode)
;; Disable electric-indent-mode
(add-hook 'haskell-mode-hook
          (lambda ()
            (electric-indent-mode -1)))

;; Check haskell-mode info page if anything breaks!
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-file)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-interactive-switch)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)))

;;; ORG-MODE
(require 'org)
(require 'org-habit)

(setq org-agenda-files `(,org-directory))
;; state logging for org-habit (! => with timestamp) (@ => timestamp + note)
(setq org-todo-keywords
      '((sequence "TODO" "DONE(!)")))

(add-to-list 'org-modules 'org-habit)

;; org-habit settings
(setq org-habit-following-days 6
      org-habit-preceding-days 21
      org-habit-graph-column 50)
;; browser-url
(setq browse-url-browser-function 'browse-url-chromium)

;; org-capture
(setq org-capture-templates
      `(("i" "Scheduled TODO" entry (file+headline "main.org" "Tasks")
         "* TODO %?\n  SCHEDULED: %^t"
         :kill-buffer t)
          ("j" "Journal" entry (file+datetree "journal.org")
           ,(concat "* %? %^g           \n\n"
                    "╭──────────────      \n"
                    " Entered on %U       \n"
                    " Was in: [[%F][%f]]  \n"
                    " ──────────────      \n"
                    " %i                  \n"
                    "╰──────────────        ")
           :kill-buffer t)
          ("n" "Note" entry (file+headline "notes.org" "Notes")
           "* %?\n "
           :kill-buffer t)
          ("h" "Habit" entry (file+headline "habits.org"  "Habits")
           ,(concat "* TODO %?\n" 
                    "  SCHEDULED: <%(read-date \"%Y-%m-%d %a\") .+%^{Repeat every|1d|1w|1m|}> \n"
                    "  :PROPERTIES:       \n"
                    "  :STYLE:    habit   \n"
                    "  :END:              \n")
           :kill-buffer t)
          ("b" "Birthday" plain (file+headline "remember.org" "Birthdays")
           "\%\\%(org-anniversary %(read-date)) %?"
           :kill-buffer t)
          ("a" "Anniversary" plain (file+headline "remember.org" "Anniversaries")
           "\%\\%(org-anniversary %(read-date)) %?"
           :kill-buffer t)))

;; org-mode inline image size
(setq org-image-actual-width nil)

;; bindings | rarely used
(add-hook 'org-mode-hook
          (lambda ()
            ;; bindings
            (local-set-key (kbd "M-<return>") 'org-insert-subheading)
            (local-set-key (kbd "C-c p") 'org-pomodoro)
            (local-set-key (kbd "C-c a") 'org-agenda)))


;;; POMODORO
;; pomodoro hooks for awesome notifications
(add-hook 'org-pomodoro-finished-hook
          (lambda ()
            (notify "Break!")))
(add-hook 'org-pomodoro-started-hook
          (lambda ()
            (notify "Start!")))
(add-hook 'org-pomodoro-break-finished-hook
          (lambda ()
            (notify "Over!")))

;;; C-MODE
;; Use k&r coding style as default
(setq c-default-style "k&r")
;; Use Linus's style while editing a file in linux-folder
(setq linux-folder "~/code/linux/")

;; Imported from linux/Documentation/CodingStyle
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name linux-folder)
                                       filename))
                (setq c-basic-offset 8
                      tab-width 8
                      indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))
;; end import

;; my own c-mode hook function
(add-hook 'c-mode-common-hook
          (lambda ()
            (local-set-key (kbd "RET")
                           'newline-and-indent)
            (whitespace-mode)))


;;; C++-MODE
(c-add-style "cpp-style"
             '("stroustrup"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               ))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "cpp-style")))


;;; PYTHON-MODE
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)
(require 'python-mode)

;;; LISP MODE
(setq inferior-lisp-program "/usr/bin/clisp")
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)          

;;; JAVA MODE
(add-hook 'java-mode-hook 'easy-auto-complete-mode-hook)

;;; RUBY MODE
(autoload 'inf-ruby "inf-ruby" "Run on inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;; LUA MODE
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;; MAGIT
(setq magit-auto-revert-mode nil)

;;; ERC
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(setq erc-autojoin-channels-alist '(("freenode.net"
                                     "#emacs" "#haskell" "#glugnith" "#c++"
                                     "#clojure" "#scala" "#javascript"
                                     "#archlinux" "#xmonad" "#c" "#bash"
                                     "#git" "#fp@nith" "#lisp" "#clojure"
                                     "#scheme" "#elm")))
(setq erc-prompt-for-nickserv-password nil)
(if (boundp 'my-freenode-nickserv-password)
    (setq erc-nickserv-password
          `((freenode (("narendraj9" . ,my-freenode-nickserv-password))))))
;; Do not switch buffers on connecting
(setq erc-join-buffer 'bury)
(add-hook 'erc-mode-hook (lambda ()
                           (interactive)
                           ;; Override the functions that sends notifications #TOFIX
                           (eval-after-load "erc-desktop-notifications"
                             '(defun erc-notifications-notify (nick msg)
                                (notify (format "%s said: %s"
                                                nick msg)
                                        10
                                        20)))
                           (erc-notifications-mode)
                           (set-face-attribute 'erc-default-face nil
                                               :foreground "papaya whip")
                           (set-face-attribute 'erc-input-face nil
                                               :foreground "burlywood")))
;;; EMACS-SERVER
;; start emacs-server only if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

;;; For MS-WINDOWS
(if (eq system-type 'windows-nt)
    (progn
      ;; Default directory on windows isn't ~/
      (setq default-directory (expand-file-name "~/"))
      (setq interprogram-paste-function 'x-selection-value)
      ;; for some reason, selection highlight isn't turned on by default
      (transient-mark-mode t)))
