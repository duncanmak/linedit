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