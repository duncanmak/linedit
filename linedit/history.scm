;;; -*- Mode: Scheme; scheme48-package: history -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.

(define max-history 100)
(define max-undo    100)

(define keep-duplicates #f)
(define keep-blanks #t)

(define-record-type history
  (%make-history %line %edit)
  session?
  (%line history:line)
  (%edit history:edit))

(define-record-discloser history
  (lambda (h)
    `(History ,(history:line h) ,(history:edit h))))

(define (make-history . args)
  (let-optionals args ((hist (make-ring-buffer max-history))
                       (undo (make-ring-buffer max-undo)))
    (%make-history hist undo)))

(define (empty-history)
  (%make-history (make-ring-buffer 0)
                 (make-ring-buffer 0)))

(define (add-line-history history s)
  (let* ((h (history:line history))
         (i (ring-buffer:peek h)))
    (cond
     ((and (string-null? s)
           (not keep-blanks)) '())
     ((and (string? i)
           (string=? s i)
           (not keep-duplicates)) '())
     (else (ring-buffer:add h s)))))

(define (retrieve h accessor)
  (let ((r (accessor h)))
    (if (string? r) r '())))

(define (get-line-history hist direction)
  (case direction
    ((next)     (retrieve (history:line hist) ring-buffer:next))
    ((previous) (retrieve (history:line hist) ring-buffer:previous))
    (else (error "this is not valid" direction))))