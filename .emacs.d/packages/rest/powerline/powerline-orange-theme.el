;;; -*- lexical-binding: t  -*-
;;; powerline-custom-theme.el | Modified Themes for Powerline

;; Copyright (C) 2012-2013 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.0
;; Keywords: mode-line

;;; Code:

(defcustom powerline-display-buffer-size t
  "When non-nil, display the buffer size."
  :type 'boolean)

(defcustom powerline-display-mule-info t
  "When non-nil, display the mule info."
  :type 'boolean)

(defcustom powerline-display-hud t
  "When non-nil, display the hud."
  :type 'boolean)

;; Override powerline colors
(defface custom-active1
  '((t (:foreground "white" :background "grey9" :inherit mode-line)))
  "Powerline face 1."
  :group 'custom-powerline)

(defface custom-active2
  '((t (:foreground "white" :weight bold :background "grey40" :inherit mode-line)))
  "Powerline face 2."
  :group 'custom-powerline)

(defface custom-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'custom-powerline)

(defface custom-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'custom-powerline)

;;; ###autoload
(defun powerline-orange-theme ()
  "Setup the default mode-line."
  (interactive)
  (set-face-attribute 'mode-line nil
		      :weight 'bold
		      :foreground "Black"
		      :background "DarkOrange"
		      :box '(:color "firebrick4" :style 'sunken))
  (setq powerline-default-separator 'curve)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'custom-active1 'custom-inactive1))
                          (face2 (if active 'custom-active2 'custom-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
							  (powerline-current-separator)
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           (powerline-current-separator)
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (when powerline-display-buffer-size
                                       (powerline-buffer-size nil 'l))
                                     (when powerline-display-mule-info
                                       (powerline-raw mode-line-mule-info nil 'l))
                                     (powerline-buffer-id nil 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when (boundp 'erc-modified-channels-object)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
				     (unless window-system
				       (powerline-raw (char-to-string #xe0a1) face1 'l))
				     (powerline-raw "%2l" face1 'l)
				     (powerline-raw ":" face1 'l)
				     (powerline-raw "%3c" face1 'r)
				     (funcall separator-right face1 mode-line)
				     (powerline-raw " ")
				     (powerline-raw "%5p" nil 'r)
                                     (when powerline-display-hud
                                       (powerline-hud face2 face1)))))
		     (concat (powerline-render lhs)
			     (powerline-fill face2 (powerline-width rhs))
			     (powerline-render rhs)))))))

(provide 'powerline-orange-theme)

;;; powerline-custom-theme.el ends here
