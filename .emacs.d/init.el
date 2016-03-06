                                            ;;;;;;;;;;;;;;;;;;
                                            ;; My init.el   ;;
                                            ;;;;;;;;;;;;;;;;;;
;;; PACKAGE ARCHIVES
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(setq package-user-dir (expand-file-name "packages/elpa"
                                         user-emacs-directory))
;; Add Customized packages to load-path
(let ((default-directory (expand-file-name "packages/rest"
                                           user-emacs-directory)))
  (normal-top-level-add-to-load-path '("hledger-mode"
                                       "powerline")))
(package-initialize)

;;;APPEARANCE
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(defun set-appearance ()
  (interactive)
  "Set up the appearance of emacs."
  (progn
    ;; Make the window simpler.
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    (fringe-mode 0)

    ;; Buffer background and foreground
    (setq blackboard-theme-color "#0C1021")
    (setq black-theme-color "gray9")
    (set-background-color black-theme-color)
    (set-foreground-color "white")

    ;; Powerline
    (require 'powerline-custom-themes)

    (if (window-system) (powerline-vermilion-theme))))
(set-appearance)

;;; VARIABLES
(setq secrets-file "~/secrets.el")
(setq org-directory "~/miscellany/personal/org")
;; Blog
(setq blog-dir "~/code/blog/narendraj9.github.io")
(setq blog-posts-dir (expand-file-name "web/posts/" blog-dir))
;; hledger
(setq hledger-jfile "~/miscellany/personal/finance/accounting.journal")

;;; GLOBAL KEY BINDINGS
(global-set-key (kbd "M-[") 'backward-kill-word)
;; org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c k") 'kill-buffer-delete-window)
;; personal accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-jdo)
;; utilities
(global-set-key (kbd "C-c d") 'insert-date-at-point)
(global-set-key (kbd "C-c /") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c l") 'linum-mode)
;; rarely used bindings
(global-set-key (kbd "C-c y") 'yank-to-x-clipboard)


;;; UTILITY FUNCTION DEFINITIONS
(defun read-date (&optional format)
  "Get date from the user and return it in the format FORMAT"
  (format-time-string "%Y %m %d"
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
    (when (not (memq major-mode '(eshell-mode org-agenda)))
      (rainbow-mode))))

(defun kill-buffer-delete-window ()
  "Kill current buffer and delete its window."
  (interactive)
  "Kills the current buffer and deletes the window."
  (kill-buffer (current-buffer))
  (delete-window))

(defun cleanup-whitespace ()
  "Remove whitespaces. "
  (interactive)
  (whitespace-cleanup)
  (delete-trailing-whitespace)
  (message "Cleaned up whitespaces!"))

(defun inhibit-read-only ()
  "Because eshell is silly. Goes into read-only mode on writing over prompt."
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
  "Toggle highlight expression inside selected parens. Useful when showing code."
  (interactive)
  (if (equal show-paren-style 'parenthesis)
      (setq show-paren-style 'expression)
    (setq show-paren-style 'parenthesis)))

(defun kill-other-buffers ()
  "Kill all buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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

(defun notify (msg)
  "Notify me with a msg. Requires that dzen is installed."
  (start-process-shell-command "dzen" nil
                               (concat "echo " msg " | dzen2 -p 2 -h 200 -l 200 -fn 'Comic Sans MS:size=50'")))

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

;;; MISCELLANY
;; Keep a separate custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; load secrets.el
(if (file-exists-p secrets-file)
    (load secrets-file))

;; personal finance
(require 'hledger-mode)
(add-hook 'hledger-mode-hook 'easy-auto-complete-mode-hook)
(add-hook 'hledger-mode-hook (lambda ()
                               (flyspell-mode 1)))

;; ido
;; show completions vertically
(setq ido-decorations (quote
                       ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]"
                        " [Matched]" " [Not readable]" " [Too big]"
                        " [Confirm]")))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-auto-merge-work-directories-length -1)
(ido-mode t)

;; line numbers for rows
(global-linum-mode 0)
(setq linum-format "%2d| ")
(set-face-attribute 'linum nil
                    :background "black"
                    :foreground "steel blue")

;; tramp-mode
(setq tramp-default-method "ssh")

;; spell-checking
(dolist (hook '(markdown-mode-hook latex-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1))))

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
(global-whitespace-mode 1)
(setq whitespace-global-modes '(not erc))

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
  (setq-local ac-auto-show-menu 0.1)
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
(setq backup-directory-alist
      '(("." . "~/.autosaves/"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      )
(setq auto-save-file-name-transforms
      `((".*" ,"~/.autosaves/" t)))

;; abbrev-mode
(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(setq abbrev-file-name (expand-file-name "abbrev_defs" user-emacs-directory))
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
      org-habit-show-all-today t
      org-habit-graph-column 50)
      
;; org-capture
(setq org-capture-templates
      `(("i" "Scheduled TODO" entry (file+headline "main.org" "Today")
           "* TODO %?\n SCHEDULED: %^t")
          ("j" "Journal" entry (file+datetree "journal.org")
           ,(concat "* %? %^g             \n"
                    "╭──────────────      \n"
                    " Entered on %U       \n"
                    " Was in: [[%F][%f]]  \n"
                    " ──────────────      \n"
                    " %i                  \n"
                    "╰──────────────        "))
          ("b" "Birthday" plain (file+headline "remember.org" "Birthdays")
           "\%\\%(org-anniversary %(read-date)) %?")
          ("a" "Anniversary" plain (file+headline "remember.org" "Anniversary")
           "\%\\%(org-anniversary %(read-date)) %?")))

;; org-mode inline image size
(setq org-image-actual-width nil)

;; bindings | rarely used
(add-hook 'org-mode-hook
          (lambda ()
            ;; bindings
            (local-set-key (kbd "M-{") 'outline-previous-visible-heading)
            (local-set-key (kbd "M-}") 'outline-next-visible-heading)
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
(setq py-install-directory  (expand-file-name "elpa//python-mode-20150703.143"
                                              user-emacs-directory))
(setq-default py-shell-name "ipython2")
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)
(require 'python-mode)

;;; LISP MODE
(setq inferior-lisp-program "/usr/bin/clisp")

                            ;;; RUBY MODE
(autoload 'inf-ruby "inf-ruby" "Run on inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

;;; LUA MODE
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

;;; MAGIT
(setq magit-auto-revert-mode nil)

;;; EMACS-SERVER
;; start emacs-server only if it's not running already
(require 'server)
(unless (server-running-p)
  (server-start))

;;; For MS-WINDOWS
(if (eq system-type 'windows-nt)
    (progn
      (setq default-directory (expand-file-name "~/"))
      (setq interprogram-paste-function 'x-selection-value)))
