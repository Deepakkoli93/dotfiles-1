                                            ;;;;;;;;;;;;;;;;;;
                                            ;; My init.el   ;;
                                            ;;;;;;;;;;;;;;;;;;
;;; The Epoch
(defconst emacs-start-time (current-time))

;;; PACKAGE ARCHIVES
;;  ─────────────────────────────────────────────────────────────────
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
                                       "utils"
                                       "powerline"
                                       "mylife-mode")))
(package-initialize)

;;; VARIABLES
;;  ─────────────────────────────────────────────────────────────────
(defvar use-auto-completep
  nil
  "Boolean that decides whether auto-complete is configured and used.")

(defvar use-companyp
  t
  "Boolean that decides whether company-mode is configured and used.")

(defvar org-directory
  (expand-file-name "~/miscellany/personal/org/")
  "Directory for keeping my org-mode files.")

(defvar emacs-themes-directory
  (expand-file-name "~/.emacs.d/themes/")
  "Directory containing emacs custom themes.")

(defvar abbrev-file
  (expand-file-name "abbrev_defs"
                    user-emacs-directory)
  "File to keep abbrev definitions. ")

(defvar temp-files-directory
  (concat user-emacs-directory "tmp/")
  "This is where I intend to keep all the state related stuff.")

(defvar backups-directory
  (expand-file-name "backups/"
                    temp-files-directory)
  "Backups everywhere isn't cool. Keep all of them in this directory.")

(defvar personal-dictionary-file 
  (expand-file-name "~/miscellany/assets/personal-dict.en.pws")
  "File to keep words that I think should be a part of my dictionary")

;; Blog
(defvar blog-dir
      (expand-file-name "~/code/blog/narendraj9.github.io"))
(defvar blog-posts-dir
      (expand-file-name "web/posts/" blog-dir))
;; Hledger
(defvar hledger-jfile
      (expand-file-name "~/miscellany/personal/finance/accounting.journal"))
(when (boundp 'my-hledger-service-fetch-url)
  (setq hledger-service-fetch-url
	my-hledger-service-fetch-url))

(defvar toggle-reading-mode nil
  "Buffer local variable to keep track of reading mode state.")
(make-variable-buffer-local 'toggle-reading-mode)

;;;APPEARANCE
;;  ─────────────────────────────────────────────────────────────────
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

    ;; Pretty window divier
    (set-face-background 'vertical-border "peru")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))

    ;; Window margins | 2 pixels on each side
    (fringe-mode 2)
    (set-face-attribute 'fringe nil :background "#2E2920" :foreground "#2E2920")

    ;; Load my custom theme { Some of the lines above may be redundant }
    (add-hook 'after-init-hook (lambda ()
                                 (interactive)
                                 (load-theme 'vicarie-and-blackboard t)))
    
    ;; Powerline
    (require 'powerline)
    (require 'powerline-vermilion-theme)
    (if (window-system) (powerline-vermilion-theme))))
    
(set-appearance)

;;; GLOBAL KEY BINDINGS
;;  ─────────────────────────────────────────────────────────────────
;; Keeping things in reach!
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
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-g f") 'avy-goto-char-timer)
(global-set-key (kbd "C-c w") 'switch-to-window)

(global-set-key (kbd "C-c =") 'vicarie/eval-print-last-sexp)
(global-set-key (kbd "C-c +") 'vicarie/eval-replace-last-sexp)
(global-set-key (kbd "C-c q") 'fill-paragraph-and-move-forward)

(global-set-key (kbd "C-c D") 'insert-date-at-point)
(global-set-key (kbd "C-c L") 'linum-mode)

(global-set-key (kbd "C-c d") 'define-word-at-point)
(global-set-key (kbd "C-c s") 'show-synonyms-for-word-at-point)

(global-set-key (kbd "C-c u") 'enlarge-current-window)
(global-set-key (kbd "C-c y") 'yank-to-x-clipboard)
(global-set-key (kbd "C-c i") 'go-back-to-intellij)

;;; UTILITY FUNCTION DEFINITIONS
;;  ─────────────────────────────────────────────────────────────────
(defun switch-to-window (direction)
  "Switch to another window with vim like keys."
  (interactive "c")
  (pcase direction
    (?h (windmove-left))
    (?j (windmove-down))
    (?k (windmove-up))
    (?l (windmove-right))
    (other (ido-jump-to-window))))

(defun ido-jump-to-window ()
  "This is shamelessly copied from the Emacs Wiki.
I will rewrite it to mak it simpler"
  (interactive)
  (let* ((swap (lambda (l)
                 (if (cdr l)
                     (cons (cadr l) (cons (car l) (cddr l)))
                   l)))
         (visible-buffers (funcall
                           swap
                           (mapcar
                            (lambda (window)
                              (buffer-name (window-buffer window)))
                            (window-list))))
         (buffer-name (ido-completing-read "Window: " visible-buffers))
         window-of-buffer)
    (if (not (member buffer-name visible-buffers))
        (error "'%s' does not have a visible window" buffer-name)
      (setq window-of-buffer
            (delq nil (mapcar
                       (lambda (window)
                         (if (equal buffer-name
                                    (buffer-name (window-buffer window)))
                             window
                           nil))
                       (window-list))))
      (select-window (car window-of-buffer)))))

(defun toggle-reading-mode ()
  "Set up the current window for reading."
  (interactive)
  (if (not toggle-reading-mode)
      (let* ((width (window-text-width))
             (reading-pane-width 80)
             (margin-width (round (/ (- width reading-pane-width) 2.0))))
        (setq left-margin-width margin-width
              right-margin-width left-margin-width)
        (visual-line-mode)
        (fill-region (point-min) (point-max))
        (utils-easy-move-mode)
        ;; Re-set the window buffer to display changes
        (set-window-buffer (selected-window) (current-buffer))
        ;; Cannot do ^^ after read-only-mode
        (view-mode)
        (setq toggle-reading-mode t))
    (view-mode -1)
    (setq left-margin-width 0
          right-margin-width left-margin-width)
    (visual-line-mode -1)
    (utils-easy-move-mode -1)
    (set-window-buffer (selected-window) (current-buffer))
    (setq toggle-reading-mode nil)))

(defun lookup-word (word dict fallback-function)
  "Lookup a given WORD in the dictionary DICT or fallback to FALLBACK-FUNCTION.
Currently I think the online dictionary is more useful so I have
an invalid command name 'sdcv-invalid'. "
  (message "%s" word)
  (if  (executable-find "sdcv-invalid")
      (popup-tip (shell-command-to-string
                  (format "sdcv -nu \"%s\" %s %s"
                          (shell-quote-argument dict)
                          (shell-quote-argument word)
                          " | tail -n +5 ")))
    (funcall fallback-function word)))

(defun lookup-word-at-point (dict fallback-function)
  "Generic helper function for `define-word-at-point' and 
`show-synonyms-for-word-at-point'."
  (let* ((word (word-at-point)))
    (cond (mark-active
           (lookup-word (buffer-substring (mark) (point)) dict fallback-function))
          (word
           (lookup-word word dict fallback-function))
          (t
           (lookup-word
              (read-from-minibuffer "No word at point. Enter word: ")
              dict
              fallback-function)))))

(defun define-word-at-point ()
    "Shows the definition of the word at point.
Assumes that popup.el is already loaded, wordnet dictionary is available
and sdcv is installed."
    (interactive)
    (lookup-word-at-point "WordNet" 'mylife-define-word))

(defun show-synonyms-for-word-at-point ()
  "Shows synonyms similar to `define-word-at-point'"
  (interactive)
  (lookup-word-at-point "Moby Thesaurus II" 'mylife-find-synonyms))

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

(defun vicarie/eval-last-sexp-and-do (f)
  "Eval the last sexp and call f on its value"
  (let ((standard-output (current-buffer))
        (value (eval-last-sexp nil)))
    (funcall f value)))

(defun vicarie/eval-print-last-sexp ()
    "Evaluate and print the last sexp on the same line."
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (insert (format " [= %s ] " value)))))

(defun vicarie/eval-replace-last-sexp ()
  "Evaluate and replace last sexp with its value. "
  (interactive)
  (vicarie/eval-last-sexp-and-do (lambda (value)
                                   (backward-kill-sexp)
                                   (insert (format "%s" value)))))

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
  (save-excursion
    (let ((begin (point-min))
          (end (progn
                 (goto-char (point-max))
                 (backward-word)  ; Upto the line containing a word
                 (end-of-line)
                 (point))))
      (add-text-properties begin end
                         '(read-only t rear-nonsticky t front-sticky t)))))

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

;;; Defunctional Playground 
;;  ─────────────────────────────────────────────────────────────────
(defun take-notes ()
  (interactive)
  "Quick go to the notes file. "
  (find-file (expand-file-name "notes.org" org-directory))
  (goto-char (point-max)))

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

(defun screenshot-frame (window-id)
  "Take a screenshot of 400x200 pixels of the Emacs frame.
Taken from Chris Done's config"
  (shell-command-to-string
   (concat "import -window "
           (shell-quote-argument window-id)
           " +repage /tmp/frames/`date +%s`.png")))

(defun start-recording-window ()
  "Record screenshots of the window and prepare a gif."
  (interactive)
  (message "Click on the window you want to record")
  (if (file-directory-p "/tmp/frames/")
      (delete-directory "/tmp/frames/" t))
  (make-directory "/tmp/frames/" t)
  (blink-cursor-mode -1)
  (let* ((window-id  (trim (shell-command-to-string
                            "xwininfo | grep 'Window id' | cut -d ' ' -f 4")))
         ;; Take a screenshot if I am idle for 1 second
         (timer (run-with-idle-timer 0.5
                                     t
                                     `(lambda ()
                                        (screenshot-frame ,window-id)))))
    (message "Started recording... [C-c x : Stop recording]")
    (global-set-key
     (kbd "C-c x")
     `(lambda ()
        (interactive)
        (cancel-timer ,timer)
        (message "Stopped recording!")
        (blink-cursor-mode 1)
        (global-unset-key (kbd "C-c x"))
        (message (shell-command-to-string
                  (concat "convert -delay 70 /tmp/frames/*.png /tmp/frames/out.gif && "
                          "echo Ouput saved to /tmp/frames/out.gif &")))))))

;;; MISCELLANY
;;  ─────────────────────────────────────────────────────────────────
;; Make directories if not available already
(make-directory temp-files-directory t)
(make-directory backups-directory t)

;; Keep all state in ~/.emacs.d/tmp/
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" temp-files-directory)
      bookmark-default-file (expand-file-name "bookmarks"  temp-files-directory)
      eshell-directory-name (expand-file-name "eshell/"    temp-files-directory)
      url-configuration-directory (expand-file-name "url/" temp-files-directory)
      server-auth-dir (expand-file-name "server/"          temp-files-directory)
      recentf-save-file (expand-file-name "recentf"        temp-files-directory)
      ido-save-directory-list-file (expand-file-name "ido.last"
                                                     temp-files-directory))

(setq secrets-file
  (expand-file-name "~/miscellany/assets/secrets.el"))

(setq custom-file
  (expand-file-name "custom.el" user-emacs-directory))

;; Load secrets.el if available
(if (file-exists-p secrets-file)
    (load secrets-file))

;; Load  custom.el
(if (file-exists-p custom-file)
    (load custom-file))

;; I think I will mostly want the pointer to go to the end with M-r
;; And then I would do a M-l to re-center it. Since both of them use
;; `recenter-positions'. I am using advices.
(setq recenter-positions '(bottom top middle))
(advice-add 'recenter-top-bottom
            :around (lambda (f &rest args)
                      (let ((recenter-positions '(middle top bottom)))
                        (apply f args))))

;; get quick emacs key binding suggestions
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1) 

;; avy
(require 'avy)

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
(setq linum-format "%2d│")
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
(add-hook 'Man-mode-hook 'utils-easy-move-mode)
;; width of man pages
(setenv "MANWIDTH" "80")
;; Maximize emacs on startup
(if (window-system)
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))

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
;; company-mode
(when (and (boundp 'use-companyp)
           use-companyp)
  (require 'company)
  (setq company-global-modes '(hledger-mode java-mode))
  (setq company-idle-delay 0.3)
  (global-company-mode)
  (add-to-list 'company-backends 'company-hledger)
  ;; ^ company-hledger should be defined before company mode is used.
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

;; auto-complete
(when (and (boundp 'use-auto-completep)
           use-auto-completep)
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
  (define-key ac-menu-map (kbd "TAB") 'ac-complete))

;; Unique buffer names
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " • ")

;; recent files menu | remote files mess things up
(add-hook 'recentf-dialog-mode-hook 'utils-easy-move-mode)
(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;; save all backup files in a fixed directory
(setq auto-save-list-file-prefix
      (concat backups-directory "autosaves-"))
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

;; Encoding
(prefer-coding-system 'utf-8)

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

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; personal finance
(require 'hledger-mode)
(add-hook 'hledger-mode-hook (lambda ()
                               (flyspell-mode 1)))
;; mylife-mode
(require 'mylife-mode)

;;; ESHELL
;;  ─────────────────────────────────────────────────────────────────
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
;;  ─────────────────────────────────────────────────────────────────
(setq fill-column 79)

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
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
;;  ─────────────────────────────────────────────────────────────────
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
          ("n" "Notes" entry (file+headline "notes.org" "Notes")
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

;; refiling across multiple levels
(setq org-refile-targets '((nil . (:maxlevel . 6))))

;; org-mode inline image size
(setq org-image-actual-width nil)

;; bindings | rarely used
(add-hook 'org-mode-hook
          (lambda ()
            ;; bindings
            (local-set-key (kbd "M-<return>") 'org-insert-subheading)
            (local-set-key (kbd "<return>") (lambda ()
                                             (interactive)
                                             (org-return t)))
            (local-set-key (kbd "C-c p") 'org-pomodoro)
            (local-set-key (kbd "C-c a") 'org-agenda)))


;;; POMODORO
;;  ─────────────────────────────────────────────────────────────────
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
;;  ─────────────────────────────────────────────────────────────────
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
;;  ─────────────────────────────────────────────────────────────────
(c-add-style "cpp-style"
             '("stroustrup"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               ))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "cpp-style")))


;;; PYTHON-MODE
;;  ─────────────────────────────────────────────────────────────────
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)
(require 'python-mode)

;;; LISP MODE
;;  ─────────────────────────────────────────────────────────────────
(setq inferior-lisp-program "/usr/bin/clisp")

;; Key-bindings for extrerem barfing and slupring
(eval-after-load 'paredit
  `(progn
     (define-key paredit-mode-map
       (kbd "C-M-)") 'utils-paredit-slurp-all-the-way-forward)
     (define-key paredit-mode-map
       (kbd "C-M-(") 'utils-paredit-slurp-all-the-way-backward)
     (define-key paredit-mode-map
       (kbd "C-M-}") 'utils-paredit-barf-all-the-way-forward)
     (define-key paredit-mode-map
       (kbd "C-M-{") 'utils-paredit-barf-all-the-way-backward)))

(mapc (lambda (h)
        (add-hook h 'enable-paredit-mode))
      '(emacs-lisp-mode-hook lisp-mode-hook clojure-mode))

;;; RUBY MODE
;;  ─────────────────────────────────────────────────────────────────
(autoload 'inf-ruby "inf-ruby" "Run on inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;; LUA MODE
;;  ─────────────────────────────────────────────────────────────────
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;; MAGIT
;;  ─────────────────────────────────────────────────────────────────
(setq magit-auto-revert-mode nil)

;;; ERC
;;  ─────────────────────────────────────────────────────────────────
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
                           (define-key erc-mode-map
                             (kbd "C-c t") 'erc-notifications-mode)
                           (erc-notifications-mode)
                           (set-face-attribute 'erc-default-face nil
                            :foreground "papaya whip")
                           (set-face-attribute 'erc-input-face nil
                                               :foreground "burlywood")))
;;; EMACS-SERVER
;;  ─────────────────────────────────────────────────────────────────
;; start emacs-server only if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

;;; For MS-WINDOWS and OSX
;;  ─────────────────────────────────────────────────────────────────
(pcase system-type
  (`windows-nt
   ;; Default directory on windows isn't ~/
   (setq default-directory (expand-file-name "~/"))
   (setq interprogram-paste-function 'x-selection-value)
   ;; for some reason, selection highlight isn't turned on by default
   (transient-mark-mode t))
  (`darwin
   ;; Modify the CMD key to be Meta key
   (setq mac-command-modifier 'meta)
   (when (< emacs-major-version 25)
     (setq visible-bell nil)))
  (`gnu/linux
   ;; Feels like home! We can ignore the worries of the world.
   (ignore "everything" "I" "say.")))

;;; My personal island
;;; ─────────────────────────────────────────────────────────────────
;; Settings relevant only my Linux desktop at home
(when (member user-login-name '("nj" "narendraj9"))
  (set-face-attribute 'default nil
                      :family "Monaco"
                      :foundry "apple"
                      :slant 'normal
                      :weight 'normal
                      :height 98
                      :width 'normal)
  (set-fontset-font "fontset-default" nil
                    (font-spec :name "Symbola" :size 15)
                    nil 'append)
  (setq browse-url-browser-function 'browse-url-chromium))

;;; The Abiogenesis
;; ─────────────────────────────────────────────────────────────────
;; A random quote from mylife-mode in the *scratch* buffer.
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message (concat (mylife-random-quote-string)
                                      (mylife-get-auroville-quality)))

;; Show emacs startup time after init
(add-hook 'after-init-hook
          `(lambda ()
            "Show the emacs load time on startup in the echo area."
            (with-current-buffer "*scratch*"
              (make-old-content-read-only))
            (message (format "Finished loading %s in %.2f seconds."
                             ,load-file-name
                             (time-to-seconds (time-subtract (current-time)
                                                             emacs-start-time))))))
;; Keep the startup time in the echo area. 
(defun display-startup-echo-area-message ()
  "Does nothing. Says nothing. Displays nothing. That's so it."
  (ignore))
