;;; -*- Mode: Scheme; scheme48-package: (exec) -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; load.scm - Script for loading linedit
;;;

(user)
(let ((source-directory "/home/duncan/git/linedit"))
  (translate "=terminfo/" (string-append source-directory "/terminfo/"))
  (translate "=linedit/"  (string-append source-directory "/linedit/"))
  (translate "=tests/"    (string-append source-directory "/tests/")))

(config '(load "=terminfo/packages.scm"
               "=linedit/interfaces.scm"
               "=linedit/packages.scm"
               "=tests/repl.scm"))

(open 'terminfo)
(open 'linedit)
(open 'repl)

;;; Misc
(load "=tests/showkeys.scm"
      "=tests/echo.scm")