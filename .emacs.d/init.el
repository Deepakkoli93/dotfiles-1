                                            ;;;;;;;;;;;;;;;;;;
                                            ;; My init.el   ;;
                                            ;;;;;;;;;;;;;;;;;;

(setq package-user-dir (expand-file-name "packages/elpa"
                                         user-emacs-directory))

;; For some reason I don't know
;; (normal-top-level-add-subdirs-to-load-path) isn't working!
(let ((default-directory (expand-file-name "packages/rest"
                                           user-emacs-directory)))
  (normal-top-level-add-to-load-path '("hledger-mode"
                                       "powerline")))

;; Custom Themes | Maybe someday!
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; package management
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; Keep a separate custom.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

                             ;; VARIABLES
(setq secrets-file "~/secrets.el")

;; personal finance
(setq fjournal-file "~/miscellany/personal/finance/accounting.journal")

;; hakyll blog
(setq myspace-dir "/datastore/Documents/myspace")
(setq blog-dir "~/code/blog/narendraj9.github.io")
(setq blog-posts-dir (expand-file-name "web/posts/" blog-dir))

;; load secrets.el | May use it in future.
(if (file-exists-p secrets-file)
    (load secrets-file))

                            ;; APPEARANCE
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
    (require 'powerline)
    (require 'powerline-custom-theme)
    (setq powerline-default-separator 'curve)
    (if (window-system) (powerline-custom-theme))

    ;; mode-line color
    (set-face-attribute 'mode-line nil
                        :weight 'bold
                        :foreground "Black"
                        :background "DarkOrange"
                        :box '(:color "firebrick4" :style 'sunken))))
(set-appearance)

                        ;; GLOBAL KEY BINDINGS
(global-set-key (kbd "M-[") 'backward-kill-word)
(global-set-key (kbd "C-c r") 'recentf-open-files)
(global-set-key (kbd "C-c o") 'org-todo-list)
(global-set-key (kbd "C-c e") 'easy-move)
(global-set-key (kbd "C-c u") 'uneasy-move)
(global-set-key (kbd "C-c w") '(lambda ()
                                 (interactive)
                                 "Remove whitespaces. "
                                 (whitespace-cleanup)
                                 (delete-trailing-whitespace)
                                 (message "Cleaned up whitespaces!")))
(global-set-key (kbd "C-c k") '(lambda ()
                                  (interactive)
                                  "Kills the current buffer and deletes the window."
                                  (kill-buffer (current-buffer))
                                  (delete-window)))
(global-set-key (kbd "C-c y") 'yank-to-x-clipboard)

;; So that I can use it in eshell
(defun myspace ()
  (interactive)
  "Go to myspace directory"
  (cd myspace-dir))
(global-set-key (kbd "C-c m") 'myspace)

(global-set-key (kbd "C-c /") '(lambda ()
                                 (interactive)
                                 "Comment-or-uncomment-region"
                                 (comment-or-uncomment-region (region-beginning) (region-end))))

(global-set-key (kbd "C-c d") '(lambda ()
                                 (interactive)
                                 "Insert date at point."
                                 (insert-date-at-point)))

(global-set-key (kbd "C-c l") '(lambda ()
                                 (interactive)
                                 "Enable linum-mode."
                                 (linum-mode)))


                   ;; Utility Function Definitions
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
  (find-file file))

(defun insert-date-at-point ()
  "Insert current date at the current position of point."
  (interactive)
  (insert (format-time-string "%Y-%m-%d")))

;; I may need to change it on re-installation
(defun notify (msg)
  "Notify me with a msg"
  (start-process-shell-command "dzen" nil
   (concat "echo " msg " | dzen2 -p 2 -h 200 -l 200 -fn 'Monaco:size=50'")))

;; Setup an emacs window into 70-30% horizontally.
(fset 'split-thirty-seventy
      "\C-x2\C-u4\C-x^\C-u4\C-x^")
(defun split-and-shell ()
  "Split the buffer vertically and shart shell in one of the windows."
  (interactive)
  (execute-kbd-macro (symbol-function 'split-thirty-seventy))
  (other-window 1)
  (eshell))


                           ;; GENERAL STUFF
;; personal finance
(require 'hledger-mode)
(add-hook 'hledger-mode-hook 
          (lambda ()
            (auto-complete-mode 1)
            (setq ac-auto-start t)
            (define-key ac-mode-map (kbd "TAB") 'auto-complete)
            (setq ac-disable-inline nil)))


;; ido
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
(add-hook 'markdown-mode-hook
          (lambda ()
            (flyspell-mode 1)))
(add-hook 'latex-mode-hook
          (lambda ()
            (flyspell-mode 1)))

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
; erc sets all faces to default when whitespace-mode enabled => no colors
(setq whitespace-global-modes '(not erc))

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (expand-file-name "snippets/" user-emacs-directory))
(yas-global-mode 1)


;; Auto-complete stuff
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(ac-flyspell-workaround)

;; Do not complete automatically
;; Auto-complete trigger on Meta-Tab
(setq ac-auto-start nil)
(define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
;; Inline completion suggestions are distracting!
(setq ac-disable-inline t)

;; Key bindings for auto-complete-menu
(setq ac-use-menu-map t)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)
  
;; Unique buffer names:
(require 'uniquify)
(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " • ")

;; Use rainbow-mode in every buffer
(define-globalized-minor-mode global-rainbow-mode rainbow-mode
  (lambda ()
    (when (not (eq major-mode 'eshell-mode))
      (rainbow-mode))))
(global-rainbow-mode 1)

;; Rainbow parentheses
(require 'rainbow-delimiters)
(defun rainbow-delimiters-colors ()
  (set-face-foreground 'rainbow-delimiters-depth-1-face "dark red")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "dark green")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "deep pink")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "yellow")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "green")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "light blue")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "orange")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "slate blue")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "light gray")
  (set-face-foreground 'rainbow-delimiters-unmatched-face "white"))
(add-hook 'rainbow-delimiters-mode-hook 'rainbow-delimiters-colors)
(rainbow-delimiters-mode)

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

;; quitely ignore if the abbrev_defs file is missing
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


                              ;; ESHELL
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

                           ;; HASKELL-MODE

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


                             ;; ORG-MODE
(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "M-{") 'outline-previous-visible-heading)
            (local-set-key (kbd "M-}") 'outline-next-visible-heading)
            (local-set-key (kbd "C-c p") 'org-pomodoro)
            (local-set-key (kbd "C-c a") 'org-agenda)))

;; pomodoro hooks for awesome notifications
(add-hook 'org-pomodoro-finished-hook
	  (lambda ()
	    (notify "BREAK")))
(add-hook 'org-pomodoro-started-hook
	  (lambda ()
	    (notify "START")))

(setq org-agenda-files '("~/org"))
;; org-mode inline image size
(setq org-image-actual-width nil)


                              ;; C-MODE

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


                             ;; C++-MODE
(c-add-style "cpp-style"
             '("stroustrup"
               (indent-tabs-mode . nil)
               (c-basic-offset . 4)
               ))
(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-style "cpp-style")))


                            ;; PYTHON-MODE

(setq py-install-directory  (expand-file-name "elpa//python-mode-20150703.143"
                                              user-emacs-directory))
(setq-default py-shell-name "ipython2")
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
(setq py-split-windows-on-execute-p nil)
(setq py-smart-indentation t)
(require 'python-mode)
                             ;; RUBY MODE
(autoload 'inf-ruby "inf-ruby" "Run on inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

                             ;; LUA MODE
(setq auto-mode-alist (cons '("\.lua$" . lua-mode) auto-mode-alist))
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)

                               ;; MAGIT
(setq magit-auto-revert-mode nil)

                           ;; EMACS-SERVER
;; start emacs-server only if it's not running already
(if (not (and (boundp 'server-process)
              (memq (process-status server-process)
                    '(connect listen open run))))
    (server-start))
(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (with-selected-frame frame
                  (set-appearance)))))
                  

                              ;; For MS-WINDOWS
(if (eq system-type 'windows-nt)
    (progn
      (setq default-directory (expand-file-name "~/"))
      (setq interprogram-paste-function 'x-selection-value)))
            

