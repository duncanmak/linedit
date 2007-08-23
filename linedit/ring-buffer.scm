;;; -*- Mode: Scheme; scheme48-package: ring-buffer -*-
;;;
;;; ring-buffer.scm: Ring buffer abstraction
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; The 'ring-buffer' record implements a ring buffer using a
;;; vector. It is used by the command history mechanism.
;;;

(define-record-type ring-buffer
  (%make-ring-buffer %buffer %cursor %index)
  ring-buffer?
  (%buffer items)
  (%cursor cursor set-cursor!)
  (%index  index  set-index!))

(define-record-discloser ring-buffer
  (lambda (b) `(Ring-buffer ,(rb-length b) ,(rb->list b))))

(define (make-ring-buffer size . args)
  (let-optionals args ((fill 0))
    (%make-ring-buffer (make-vector size fill) 0 0)))

(define (rb-length buffer) (vector-length (items buffer)))

(define (rb->list buffer)
  (let* ((current (cursor buffer))
         (vec     (items buffer))
         (length  (vector-length vec)))
    (let loop ((i 0)
               (results '()))
      (cond ((>= i length) (reverse results))
            ((= i current) (loop (+ 1 i) (cons (list (vector-ref vec i)) results)))
            (else (loop (+ 1 i) (cons (vector-ref vec i) results)))))))

(define (increment buffer accessor)
  (let ((current (+ 1 (accessor buffer)))
        (size    (ring-buffer:length buffer)))
    (if (>= current size)
        (- current size)
        current)))

(define (ring-buffer:add buffer item)
  (let ((vec (items buffer)))
    (vector-set! vec (index buffer) item)
    (set-index! buffer (increment buffer index))))

(define (ring-buffer:length buffer)
  (vector-length (items buffer)))

(define (ring-buffer:get buffer)
  (let* ((idx  (cursor buffer))
         (vec  (items buffer))
         (item (vector-ref vec idx)))
    (set-cursor! buffer (increment buffer cursor))
    item))

(define (ring-buffer:peek buffer)
  (let* ((cursor (cursor buffer))
         (vec    (items buffer)))
    (vector-ref vec cursor)))