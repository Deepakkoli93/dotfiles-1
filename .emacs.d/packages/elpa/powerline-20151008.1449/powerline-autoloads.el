;;; powerline-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "powerline" "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline.el"
;;;;;;  "88245808d5c5f2fc4d16226725a81508")
;;; Generated autoloads from ../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline.el

(autoload 'powerline-hud "powerline" "\
Return an XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH.

\(fn FACE1 FACE2 &optional WIDTH)" nil nil)

(autoload 'powerline-mouse "powerline" "\
Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING.

\(fn CLICK-GROUP CLICK-TYPE STRING)" nil nil)

(autoload 'powerline-concat "powerline" "\
Concatonate STRINGS and pad sides by spaces.

\(fn &rest STRINGS)" nil nil)

(autoload 'defpowerline "powerline" "\
Create function NAME by wrapping BODY with powerline padding an propetization.

\(fn NAME BODY)" nil t)

(autoload 'powerline-raw "powerline" "\
Render STR as mode-line data using FACE and optionally PAD import on left (l) or right (r).

\(fn STR &optional FACE PAD)" nil nil)

(autoload 'powerline-fill "powerline" "\
Return empty space using FACE and leaving RESERVE space on the right.

\(fn FACE RESERVE)" nil nil)
 (autoload 'powerline-major-mode "powerline")
 (autoload 'powerline-minor-modes "powerline")
 (autoload 'powerline-narrow "powerline")
 (autoload 'powerline-vc "powerline")
 (autoload 'powerline-buffer-size "powerline")
 (autoload 'powerline-buffer-id "powerline")
 (autoload 'powerline-process "powerline")
 (autoload 'powerline-selected-window-active "powerline")

;;;***

;;;### (autoloads nil "powerline-themes" "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-themes.el"
;;;;;;  "64e8b0a0ad0b4e6f41152dfce6118319")
;;; Generated autoloads from ../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-themes.el

(autoload 'powerline-default-theme "powerline-themes" "\
Setup the default mode-line.

\(fn)" t nil)

(autoload 'powerline-center-theme "powerline-themes" "\
Setup a mode-line with major and minor modes centered.

\(fn)" t nil)

(autoload 'powerline-vim-theme "powerline-themes" "\
Setup a Vim-like mode-line.

\(fn)" t nil)

(autoload 'powerline-nano-theme "powerline-themes" "\
Setup a nano-like mode-line.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-autoloads.el"
;;;;;;  "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-pkg.el"
;;;;;;  "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-separators.el"
;;;;;;  "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline-themes.el"
;;;;;;  "../../../../../.emacs.d/packages/elpa/powerline-20151008.1449/powerline.el")
;;;;;;  (22172 30700 687000 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; powerline-autoloads.el ends here
