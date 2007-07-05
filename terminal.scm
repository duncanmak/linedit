;;; -*- Mode: Scheme; scheme48-package: linedit -*-
;;;
;;; terminal.scm: Terminal abstraction
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define (call-with-raw-terminal port thunk)
  (let ((orig (tty-info port))
        (raw  (make-raw-tty-info port)))
    (set-tty-info/flush port raw)
    (thunk)
    (set-tty-info/flush port orig)))

(define-syntax with-raw-terminal
  (syntax-rules ()
    ((with-raw-terminal body ...)
     (call-with-raw-terminal (current-output-port) (lambda () body ...)))))
