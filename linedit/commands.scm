;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (insert-char k l)
  (tputs (enter-insert-mode))
  (display k)
  (tputs (exit-insert-mode))
  (line-insert l k))

(define (delete-backward-char k l)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (tputs (delete-character))
             (line-remove l k))))

(define (delete-char k l)
  (if (null? (line:right l))
      l
      (begin (tputs (delete-character))
             (shift-right l))))

(define (move-beginning-of-line k l)
  (tputs (column-address 0))
  (beginning-of-line l))

(define (move-end-of-line k l)
  (tputs (column-address (+ 1 (line-length l))))
  (end-of-line l))

(define (backward-char k l)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (shift-left l (prev-char l)))))

(define (forward-char k l)
  (if (null? (line:right l))
      l
      (begin (tputs (cursor-right))
             (shift-right l (next-char l)))))

(define (kill-line k l)
  (tputs (clr-eol))
  (make-line (line:left l) '()))