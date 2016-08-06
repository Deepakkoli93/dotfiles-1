;;; defuns.el --- Utility functions for my init.el   -*- lexical-binding: t; -*-

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

;; This file would contain all the fucntion defintions that I
;; originally used to keep in my init.el

;;; Code:

(defun set-appearance ()
  (interactive)
  "Set up the appearance of emacs."
  (progn
    ;; Make the window simpler.
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)

    ;; Pretty window divider
    (set-face-background 'vertical-border "peru")
    (set-face-foreground 'vertical-border (face-background 'vertical-border))

    ;; Maximize emacs on startup
    (if (window-system)
        (add-to-list 'default-frame-alist '(fullscreen . maximized)))

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
              right-margin-width 0)
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

    
(provide 'defuns)
;;; defuns.el ends here