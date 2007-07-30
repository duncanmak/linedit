;;; -*- Mode: Scheme; scheme48-package: (exec) -*-
;;;
;;; ,config ,load terminfo/packages.scm linedit/interfaces.scm linedit/packages.scm
;;;
(user)
(config '(load "terminfo/packages.scm"
               "linedit/interfaces.scm"
               "linedit/packages.scm"))

(open 'terminfo)
(open 'linedit)
(load "tests/showkeys.scm")
(load "tests/saywhat.scm")
(config '(load "tests/repl.scm"))
(open 'repl)