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

(define (make-ring-buffer size)
  (%make-ring-buffer (make-vector size) 0 0))

(define (rb-length buffer) (vector-length (items buffer)))

(define (rb-ref buffer i)
  (if (not (ring-buffer:empty? buffer))
      (vector-ref (items buffer) i))  )

(define (rb-set! buffer idx item)
  (if (not (ring-buffer:empty? buffer))
      (vector-set! (items buffer) idx item)))

(define (rb->list buffer)
  (let* ((current (cursor buffer))
         (vec     (items buffer))
         (length  (vector-length vec)))
    (if (zero? length)
        '()
        (let loop ((i        0)
                   (results  '())
                   (ellipsis #t))
          (cond ((>= i length) (reverse results))
                ((= i current)
                 (loop (+ 1 i) (cons (list (vector-ref vec i)) results) ellipsis))
                (else
                 (let ((item (vector-ref vec i)))
                   (if (eq? item (unspecific))
                       (loop (+ 1 i) (if ellipsis (cons '... results) results) #f)
                       (loop (+ 1 i) (cons item results) ellipsis)))))))))

(define (increment buffer accessor)
  (let ((current (+ (accessor buffer) 1))
        (size    (ring-buffer:length buffer)))
    (if (>= current size)
        (- current size)
        current)))

(define (decrement buffer accessor)
  (let ((current (- (accessor buffer) 1))
        (size    (ring-buffer:length buffer)))
    (if (<= current 0)
        0
        current)))

(define (ring-buffer:add buffer item)
  (let ((i (increment buffer index)))
    (rb-set!     buffer (index buffer) item)
    (set-index!  buffer i)
    (set-cursor! buffer i)))

(define (ring-buffer:empty? buffer)
  (zero? (vector-length (items buffer))))

(define (ring-buffer:length buffer)
  (vector-length (items buffer)))

(define (ring-buffer:next buffer)
  (let* ((idx  (increment buffer cursor))
         (item (rb-ref buffer idx)))
    (set-cursor! buffer idx)
    item))

(define (ring-buffer:previous buffer)
  (let* ((idx  (decrement buffer cursor))
         (item (rb-ref buffer idx)))
    (set-cursor! buffer idx)
    item))

(define (ring-buffer:peek buffer)
  (rb-ref buffer (cursor buffer)))