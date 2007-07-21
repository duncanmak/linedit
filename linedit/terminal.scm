;;; -*- Mode: Scheme; scheme48-package: linedit -*-
;;;
;;; terminal.scm: Terminal abstraction
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define (call-with-modified-tty port tty-info thunk)
  (let ((orig (tty-info port)))
    (set-tty-info/flush port tty-info)
    (thunk)
    (set-tty-info/flush port orig)))

(define (call-with-raw-terminal port thunk)
  (let ((raw (make-raw-tty-info port)))
    (call-with-modified-tty port raw thunk)))

(define-syntax with-raw-terminal
  (syntax-rules ()
    ((with-raw-terminal body ...)
     (call-with-raw-terminal (current-output-port) (lambda () body ...)))))

(define-syntax with-tty
  (syntax-rules ()
    ((with-tty tty body ...)
     (call-with-modified-tty (current-output-port) tty (lambda () body ...)))))