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
  (lambda (b) `(Ring-buffer ,(items b))))

(define (make-ring-buffer size)
  (%make-ring-buffer (make-vector size) 0 0))

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