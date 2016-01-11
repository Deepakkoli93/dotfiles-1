;;; Compiled snippets and support files for `c++-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'c++-mode
                     '(("bk" "{\n    $0\n}\n" "bk" nil nil nil nil nil nil)
                       ("codeforces" "#include <iostream>\n#include <vector>\n#include <map>\n\n\nint main(int ac, char *av[])\n{\n        $0\n        return 0;\n}" "codeforces" nil nil nil nil nil nil)
                       ("main" "main(${1:int argc, char *argv[]})\n{\n    $0\n    return 0;\n}\n" "main" nil nil nil nil nil nil)
                       ("nbk" "\n{\n    $0\n}\n" "nbk" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Mon Jul 13 18:12:04 2015
