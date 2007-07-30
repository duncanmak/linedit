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
;;; the right of the gap is in normal order.
;;;
;;; To make a new line from a string, use
;;;       STRING->LINE s
;;;
;;; Use '^' to denote the placement of the gap.
;;;
;;; To view a line as a string, use
;;;       LINE->STRING l
;;;
;;; Various helper functions are also defined here to aid the
;;; implementation of Commands.
;;;

(define-record-type line
  (%make-line left right)
  line?
  (left   line:left)
  (right  line:right))

(define (make-line . args)
  (let-optionals args ((left  '())
                       (right '()))
    (%make-line left right)))

(define (line->string l . flag)
  (let-optionals flag ((show-cursor #f))
    (if (line? l)
        (string-append (reverse-list->string (line:left l))
                       (if show-cursor "^" "")
                       (list->string (line:right l)))
        "")))

(define (line->port l)
  (let ((s (line->string l)))
    (open-input-string
     (if (and (= 1 (string-length s))
              (eof-object? (string-ref s 0)))
         ""
         s))))

(define (string->line s)
  (if (not (string-contains s "^"))
      (make-line (reverse (string->list s)))
   (let* ((split (infix-splitter "^"))
          (line  (split s))
          (left  (car  line))
          (right (cadr line)))
     (make-line (reverse (string->list left))
                (string->list right)))))

(define (get-char l direction)
  (if (null? (direction l))
      '()
      (car (direction l))))

(define (line-insert l char)
  (make-line (cons char (line:left l))
             (line:right l)))

(define (shift-left l . char)
  (make-line (cdr (line:left l))
             (append char (line:right l))))

(define (shift-right l . char)
  (make-line (append char (line:left l))
             (cdr (line:right l))))

(define (beginning-of-line l)
  (make-line '() (append (reverse (line:left l))
                         (line:right l))))

(define (end-of-line l)
  (make-line (append (reverse (line:right l))
                     (line:left l)) '()))

(define (line-length l)
  (+ (length (line:left  l))
     (length (line:right l))))

(define (process-line line)
  (with-current-input-terminal-mode 'raw
    (let loop ((c (read-char))
               (l line))
      (let ((result (process c l)))
        (if (char=? c cr)
            result
            (loop (read-char)
                  result))))))