;;; -*- Mode: Scheme; scheme48-package: linedit -*-
;;;
;;; buffer.scm: Buffer abstraction
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define (make-buffer) (make-vector 3 (list)))
(define (%buffer-prev buf) (vector-ref buf 0))
(define (%buffer-next buf) (vector-ref buf 1))
(define (%buffer-list buf) (vector-ref buf 2))
(define (set-buffer-prev! buf prev) (vector-set! buf 0 prev))
(define (set-buffer-next! buf next) (vector-set! buf 1 next))
(define (set-buffer-list! buf list) (vector-set! buf 2 list))

(define (buffer-push str buf)
  (set-buffer-list! buf (cons str (%buffer-list buf)))
  (set-buffer-next! buf (list))
  (set-buffer-prev! buf (%buffer-list buf)))

(define (buffer-previous str buf)
  (if (%buffer-prev buf)
    (let ((prev (%buffer-prev buf)))
      (set-buffer-next! buf (cons str (%buffer-next buf)))
      (set-buffer-prev! buf (cdr (%buffer-prev buf)))
      prev)))

(define (buffer-next str buf)
  (if (%buffer-next buf)
    (let ((next (%buffer-next buf)))
      (set-buffer-prev! buf (cons str (%buffer-prev buf)))
      (set-buffer-next! buf (cdr (%buffer-next buf)))
      next)))

(define (buffer-peek buf)
  (let ((prev (%buffer-prev buf)))
    (if prev
        (car prev))))

(define (buffer-cycle buf)
  (define (wrap-buffer)
    (if (null? (%buffer-prev buf))
        (begin 
          (set-buffer-next! buf (reverse (%buffer-next buf)))
          (set-buffer-prev! buf (%buffer-next buf))
          (set-buffer-next! buf (list)))))
  (wrap-buffer)
  (set-buffer-next! buf (cons (car (%buffer-prev buf)) (%buffer-next buf)))
  (set-buffer-prev! buf (cdr (%buffer-prev buf)))
  (wrap-buffer)
  #t)

(define (print-object b t)
  (format t "prev: ~a~%next: ~a~%list: ~a~%"
	  (%buffer-prev b) (%buffer-next b) (%buffer-list b))*)
