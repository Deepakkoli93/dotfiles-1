;;; Compiled snippets and support files for `haskell-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'haskell-mode
                     '(("boxed" "${1:$(make-string 80 ?-)}\n-- $1\n${1:$(make-string 80 ?-)}\n$0\n" "boxed" nil nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("boxed-beg" "{-\n @author: Narendra Joshi\n @date: `(format-time-string \"%d %b %Y\")`\n ${}\n--}\n$0\n" "boxed-beg" nil nil
                        ((yas-indent-line 'fixed))
                        nil nil nil)
                       ("{" "{-# ${1:PRAGMA} #-}" "pragma" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Jul 13 18:12:04 2015
