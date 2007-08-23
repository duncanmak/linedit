;;; -*- Mode: Scheme; scheme48-package: history -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.

(define max-history 100)
(define keep-duplicates #f)
(define keep-blanks #t)

(define *command-history* '())

(define (initialize-history)
  (set! *command-history* (make-ring-buffer max-history)))

(define (add-history s)
  (let* ((h *command-history*)
         (i (ring-buffer:peek h)))
    (cond
     ((and (string-null? s)
           (not keep-blanks)) '())
     ((and (string? i)
           (string=? s i)
           (not keep-duplicates)) '())
     (else (ring-buffer:add h s)))))

(define (get-history)
  (let* ((h *command-history*))
    (ring-buffer:get h)))