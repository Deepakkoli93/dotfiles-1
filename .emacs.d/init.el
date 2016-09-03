                                            ;;;;;;;;;;;;;;;;;;
                                            ;; My init.el   ;;
                                            ;;;;;;;;;;;;;;;;;;
;;; The Epoch
(defconst emacs-start-time (current-time))

;;; PACKAGE ARCHIVES
;;  ─────────────────────────────────────────────────────────────────
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://melpa-stable.milkbox.net/packages/"))
(setq package-user-dir (expand-file-name "packages/elpa/"
                                         user-emacs-directory))
;; Add my packages to load-path
(let ((default-directory (expand-file-name "packages/rest/"
                                           user-emacs-directory)))
  (normal-top-level-add-to-load-path '("hledger-mode"
                                       "utils"
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

(defvar emacs-lib-directory
  (expand-file-name "~/.emacs.d/lib/")
  "Path to file contaings all function definitions.")

(defvar org-directory
  (expand-file-name "~/miscellany/personal/org/")
  "Directory for keeping my org-mode files.")

(defvar emacs-themes-directory
  (expand-file-name "~/.emacs.d/themes/")
  "Directory containing emacs custom themes.")

(defvar abbrev-file
  (expand-file-name "abbrev_defs"
                    emacs-lib-directory)
  "File to keep abbrev definitions. ")

(defvar temp-files-directory
  (concat user-emacs-directory "tmp/")
  "This is where I intend to keep all the state related stuff.")

(defvar backups-directory
  (expand-file-name "backups/"
                    temp-files-directory)
  "Backups everywhere isn't cool. Keep all of them in this directory.")

(defvar desktops-directory
  (expand-file-name "desktops/"
                    temp-files-directory)
  "Directory for storing emacs desktop session files.")

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

;; LIBRARY
;;  ─────────────────────────────────────────────────────────────────
(add-to-list 'load-path emacs-lib-directory)
(require 'defuns)

;;; APPEARANCE
;;  ─────────────────────────────────────────────────────────────────
(add-to-list 'custom-theme-load-path
             emacs-themes-directory)
(set-appearance)

;;; GLOBAL KEY BINDINGS
;;  ─────────────────────────────────────────────────────────────────
;; Keeping things in reach!
(global-set-key (kbd "M-[") 'backward-kill-word)
(global-set-key (kbd "C-c m") 'switch-to-minibuffer)

;; Bindings for org-mode
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c k") 'kill-buffer-delete-window)

;; Personal Accounting
(global-set-key (kbd "C-c e") 'hledger-jentry)
(global-set-key (kbd "C-c j") 'hledger-run-command)

;; Utilities
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
(global-set-key (kbd "<print>") 'snap-it)

;; Mouse events
(global-set-key [mouse-3] 'define-word-at-point)

;;; MISCELLANY
;;  ─────────────────────────────────────────────────────────────────
;; Make directories if not available already
(make-directory temp-files-directory t)
(make-directory backups-directory t)
(make-directory desktops-directory t)

;; Keep all state in ~/.emacs.d/tmp/
(setq ac-comphist-file (expand-file-name "ac-comphist.dat" temp-files-directory)
      bookmark-default-file (expand-file-name "bookmarks"  temp-files-directory)
      eshell-directory-name (expand-file-name "eshell/"    temp-files-directory)
      url-configuration-directory (expand-file-name "url/" temp-files-directory)
      server-auth-dir (expand-file-name "server/"          temp-files-directory)
      recentf-save-file (expand-file-name "recentf"        temp-files-directory)
      desktop-path `(,desktops-directory)
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

;; Prefer newer lisp files.
(setq load-prefer-new t)

;; Encoding and default font settings
(prefer-coding-system 'utf-8)
(when (find-font (font-spec :name "Symbola"))
  (set-fontset-font "fontset-default" nil
                    (font-spec :name "Symbola" :size 15)
                    nil 'append))
(when (find-font (font-spec :name "Monaco"))
    (set-face-attribute 'default nil
                      :family "Monaco"
                      :foundry "apple"
                      :slant 'normal
                      :weight 'normal
                      :height 98
                      :width 'normal))

;; Make chromium the default browser if it is installed
(when (executable-find "chromium")
  (setq browse-url-browser-function 'browse-url-chromium))

;; PDF-tools
(add-hook 'pdf-view-mode-hook (lambda ()
                                (require 'pdf-view)
                                (unless (ignore-errors (pdf-tools-install) t)
                                  (message "Warning: pdf-tools failed to install.")
                                  (utils-easy-move-mode))))

;; Recent files menu | remote files mess things up
(add-hook 'recentf-dialog-mode-hook 'utils-easy-move-mode)
(setq recentf-auto-cleanup 'never)
(setq recentf-keep '(file-remote-p file-readable-p))
(setq recentf-max-saved-items 100)
(recentf-mode 1)

;; Save all backup files in a fixed directory
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

;; General settings
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)
(setq show-paren-delay 0)

(setq column-number-mode t)
(setq-default tab-width 4)

;; Miscellany (maybe)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t)
(setq inhibit-splash-screen t)

;; I think I will mostly want the pointer to go to the end with M-r
;; And then I would do a M-l to re-center it. Since both of them use
;; `recenter-positions'. I am using advices.
(setq recenter-positions '(bottom top middle))
(advice-add 'recenter-top-bottom
            :around (lambda (f &rest args)
                      (let ((recenter-positions '(middle top bottom)))
                        (apply f args))))

;; Get quick emacs key binding suggestions
(require 'guide-key)
(setq guide-key/guide-key-sequence t)
(guide-key-mode 1) 

;; avy
(require 'avy)

;; Line numbers for rows
(global-linum-mode 0)
(setq linum-format "%2d│")
(set-face-attribute 'linum nil
                    :background "black"
                    :foreground "steel blue")

;; Spell-checking
(dolist (hook '(markdown-mode-hook latex-mode-hook org-mode-hook))
  (add-hook hook (lambda ()
                   (flyspell-mode 1))))
(setq ispell-personal-dictionary personal-dictionary-file)
                                 
;; Unique buffer names
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " • ")

;; abbrev-mode
(setq save-abbrevs nil)
(setq-default abbrev-mode t)
(setq abbrev-file-name abbrev-file)
(if (file-exists-p abbrev-file-name)
    (quietly-read-abbrev-file))

;; Enable disabled commands
(put 'narrow-to-region 'disabled nil)

;; mylife-mode
(require 'mylife-mode)

;;; Completion at Point
;; ――――――――――――――――――――――――――――――――――――  
(cond
 ;; company-mode
 ((and (boundp 'use-companyp)
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
 ((and (boundp 'use-auto-completep)
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
 
 ;; no-mode
 (t (message "No completion enabled.")))

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
(yas-global-mode 1)

;; IDO
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

;; helm
(require 'helm-config)
;; To ignore warnings about redefinitions
(setq ad-redefinition-action 'accept)
(helm-mode 1)


;; Personal Finance
;; ―――――――――――――――――――――――――――――――――――― 
(require 'hledger-mode)
(add-hook 'hledger-mode-hook (lambda ()
                               (flyspell-mode 1)))
(hledger-enable-reporting)

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
                                            '("git" "commit" "log" "diff" "grep"))))
;;; WRITING
;;  ─────────────────────────────────────────────────────────────────
(setq fill-column 79)

;;; HASKELL-MODE
;;  ─────────────────────────────────────────────────────────────────
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

(setq org-hide-leading-stars t)
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
      `(("i" "TODO" entry (file+headline "main.org" "Tasks")
         "* TODO %?"
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
      '(emacs-lisp-mode-hook lisp-mode-hook
                             clojure-mode-hook
                             cider-repl-mode-hook
                             eshell-mode-hook))

;;; MAN-MODE
;;  ─────────────────────────────────────────────────────────────────
(add-hook 'Man-mode-hook 'utils-easy-move-mode)
(setenv "MANWIDTH" "80")

;;; WHITESPACE-MODE
;;  ─────────────────────────────────────────────────────────────────
;; For the 80-column rule
(require 'whitespace)
(setq whitespace-style '(face lines-tail))
(setq whitespace-global-modes '(not erc-mode eshell-mode org-agenda-mode Info-mode))
(global-whitespace-mode 1)

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

;; TRAMP-MODE
;;  ─────────────────────────────────────────────────────────────────
(setq tramp-default-method "ssh")
;; Make backups for tramp files in their original locations
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp nil))

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
   ;; This is very stupid of Apple keyboards
   (setq mac-right-option-modifier 'ctrl)
   ;; I don't need a fn
   (setq mac-function-modifier 'ctrl)
   (when (< emacs-major-version 25)
     (setq visible-bell nil)))
  (`gnu/linux
   ;; Feels like home! We can ignore the worries of the world.
   (ignore "everything" "I" "say.")))

;;; The Abiogenesis
;; ─────────────────────────────────────────────────────────────────
;; A random quote from mylife-mode in the *scratch* buffer.
(setq initial-major-mode 'fundamental-mode)
(setq initial-scratch-message (concat (mylife-random-quote-string)
                                      (mylife-get-auroville-quality)))

;; Show emacs startup time after init
(add-hook 'emacs-startup-hook
          `(lambda ()
            "Show the emacs load time on startup in the echo area."
            (message (format "Finished loading %s in %.2f seconds."
                             ,load-file-name
                             (time-to-seconds (time-subtract (current-time)
                                                             emacs-start-time))))
            (pop-to-buffer "*scratch*")
            (make-old-content-read-only)
            (delete-other-windows)))
;; Keep the startup time in the echo area. 
(defun display-startup-echo-area-message ()
  "Does nothing. Says nothing. Displays nothing. That's so it."
  (ignore))

