;;; mylife-mode.el --- A mode for miscellaneous tools

;;; Copyright (C) 2015-2016 Narendra Joshi [This is funny.]

;; Author: Narendra Joshi <narendraj9@gmail.com>
;; URL: 
;; Version: 0.1
;; Keywords: life
;; Package-Requires: ((cl-lib "0.2"))

;;; Commentary:
;;
;; This mode is for all the stuff that doesn't fit anywhere else and mostly
;; that stuff is related to my life and hence the name.
;; Currently, I have a relationship rating form and a few functions that I
;; like to call once in a while.


;;; Code: 

(add-to-list 'auto-mode-alist '("\\.life\\'" . life-mode))

(require 'mylife-relationships)
(require 'mylife-quotes)
(require 'mylife-dictionary)

(defgroup mylife nil
  "Customization group for mylife-mode.")

(defcustom mylife-mode-hook nil
  "Normal hook for entering mylife-mode."
  :type 'hook
  :group 'mylife)

(defcustom mylife-birth-date "1993-06-18T00:00:00+0530"
  "Your date of birth."
  :type 'string
  :group 'mylife)

(defcustom mylife-mylife-expectancy 78
  "Mylife expectancy in your country."
  :type 'integer
  :group 'mylife)

(defcustom mylife-past-box-char ?\x25A1
  "Character used for represting days/weeks that you have already lived."
  :type 'character
  :group 'mylife)

(defcustom mylife-future-box-char ?\x25A0
  "Character for the time units in futures."
  :type 'character
  :group 'mylife)
  

;;; Derivations
(setq mylife-birth-time (date-to-time mylife-birth-date))

(setq mylife-death-time (time-add mylife-birth-time
                                (days-to-time (* mylife-mylife-expectancy
                                                 365))))
(setq mylife-days-gone
            (round (time-to-number-of-days
                    (time-since mylife-birth-date))))
(setq mylife-days-left
            (round (time-to-number-of-days
                    (time-subtract mylife-death-time
                                   (current-time)))))
(setq mylife-years-gone
            (round (/ mylife-days-gone
                      365.0)))
(setq mylife-years-left
            (round (/ mylife-days-left
                      365.0)))
(setq mylife-weeks-gone
            (round (/ mylife-days-gone
                      7.0)))
(setq mylife-weeks-left
            (round (/ mylife-days-left
                      7.0)))

;;; Auxiliary functions
(defun mylife-get-display-buffer ()
  "Get/create the display buffer."
  (let ((display-buffer (get-buffer-create "*Mylife*")))
    (with-current-buffer display-buffer
      (mylife-mode)
      (local-set-key (kbd "q")
                     (lambda ()
                       (interactive)
                       (quit-restore-window (selected-window) 'kill)))
      (setq header-line-format " q : Quit ")
      (erase-buffer))
    display-buffer))

;;; Main functions
(defun mylife-draw-weeks ()
  "Draw the dichotomy of your mylife years."
  (interactive)
  (let ((mylife-buffer (mylife-get-display-buffer))
        (mylife-string (concat
                      (make-string mylife-weeks-gone
                                    mylife-past-box-char)
                      (make-string mylife-weeks-left
                                   mylife-future-box-char))))
    (with-current-buffer mylife-buffer
      (insert mylife-string))
    (pop-to-buffer mylife-buffer)
    (delete-other-windows)
    (goto-char (point-min))))

(defun mylife-log-now (desc)
  "Log something about the current time. Inserts desc into the
current buffer with a timestamp. Use diary/calender/org-mode instead."
  (interactive "sDescription: ")
  (insert (format "\n%s  ::  %s"
           (format-time-string "%F %R")
           desc)))

(defun mylife-mode-init ()
  "Function that does initial setup in the major-mode function."
  (ignore))


(defun mylife-get-auroville-quality ()
  "Return one of the Auroville qualities."
  (let ((index (string-to-number (format-time-string "%d")))
        (qualities
         '(

"
                  誠実
                Seijitsu
               Sincerity
                    
"

"
                  謙虚
                 Kenkyo
                Humility

"

"
                  感謝
                 Kansha
               Gratitude

"

"
                 忍耐力
              Nintai-ryoku
              Perseverance

"

"
                  吸引
                 Kyūin
               Aspiration

"

"
                 感受性
                Kanjusei
              Receptivity

"

"
                  進捗
               Shinchoku
                Progress

"
"
                  勇気
                  Yūki
                Courage

"
"
                   善
                  Zen
                Goodness
"

"
                 寛大さ
               Kandai-sa
               Generosity


"
"
                  平等
                 Byōdō
                Equality

"

"
                  平和
                 Heiwa
                 Peace

")))
    (propertize (nth  (mod index (length qualities)) qualities)
                'font-lock-face '(:foreground "Sienna" :height 3.0)
                'rear-nonsticky t)))
     

;;;###autoload
(define-derived-mode mylife-mode prog-mode "Mylife" ()
  "Major mode for almost anything random."
  (interactive)
  (mylife-mode-init))

(provide 'mylife-mode)

