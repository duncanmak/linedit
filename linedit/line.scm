;;; -*- Mode: Scheme; scheme48-package: line -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; Line abstraction
;;;
;;; The 'line' record implements a gap buffer. It is the workhorse and
;;; main data structure of linedit. It contains two fields: left and
;;; right. Data to the left of the gap are in reverse order, data to
;;; the right of the gap is in normal order. There is also a 'column'
;;; field for tracking where the starting column show be.
;;;
;;; To make a new line, use
;;;
;;;       MAKE-EMPTY-LINE [prompt string]
;;;
;;; To make a new line from a string, use
;;;
;;;       STRING->LINE s
;;;
;;; Use '^' to denote the placement of the gap.
;;;
;;; To view a line as a string, use
;;;
;;;       LINE->STRING l
;;;
;;; Various helper functions are also defined here to aid the
;;; implementation of Commands.
;;;
;;;

(define-record-type line
  (make-line left right col history)
  line?
  (left    line:left)
  (right   line:right)
  (col     line:column)
  (history line:history))

(define-record-discloser line
  (lambda (l)
    `(Line ,(line->string l #t))))

(define (line:length l)
  (+ (length (line:left  l))
     (length (line:right l))))

(define (make-empty-line . args)
  (let-optionals args ((prompt  "")
                       (history (empty-history)))
    (make-line '() '() (+ 1 (string-length prompt)) history)))

(define (copy-line l . args)
  (let-optionals args ((left    (line:left   l))
                       (right   (line:right  l))
                       (col     (line:column l))
                       (history (line:history l)))
    (make-line left right col history)))

(define (line->string l . flag)
  (let-optionals flag ((show-cursor #f))
    (if (line? l)
        (string-append (reverse-list->string (line:left l))
                       (if show-cursor "^" "")
                       (list->string (line:right l)))
        "")))

(define (string->line s)
  (if (not (string-contains s "^"))
      (make-line (reverse (string->list s)) '() 0 '())
      (let* ((split (infix-splitter "^"))
             (line  (split s))
             (left  (car  line))
             (right (cadr line)))
        (make-line (reverse (string->list left))
                   (string->list right)
                   0 '()))))

(define (get-char l direction)
  (if (null? (direction l))
      '()
      (car (direction l))))

(define (line-insert l char)
  (copy-line l (cons char (line:left l))
             (line:right l)))

(define (shift-left l . char)
  (copy-line l (cdr (line:left l))
             (append char (line:right l))))

(define (shift-right l . char)
  (copy-line l (append char (line:left l))
             (cdr (line:right l))))

(define (beginning-of-line l)
  (copy-line  l '()
              (append (reverse (line:left l))
                     (line:right l))))

(define (end-of-line l)
  (copy-line l (append (reverse (line:right l))
                     (line:left l))
             '()))