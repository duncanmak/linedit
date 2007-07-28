;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (lookup-meta-keymap l k)
  (process k l meta-keymap))

(define (show-newline l k)
  (newline) (display cr) l)

(define (insert-char l k)
  (tputs (enter-insert-mode))
  (display k)
  (tputs (exit-insert-mode))
  (line-insert l k))

(define (delete-backward-char l k)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (tputs (delete-character))
             (shift-left l))))

(define (delete-char l k)
  (if (null? (line:right l))
      l
      (begin (tputs (delete-character))
             (shift-right l))))

(define (move-beginning-of-line l k)
  (tputs (column-address 0))
  (beginning-of-line l))

(define (move-end-of-line l k)
  (tputs (column-address (+ 1 (line-length l))))
  (end-of-line l))

(define (backward-char l k)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (shift-left l (prev-char l)))))

(define (forward-char l k)
  (if (null? (line:right l))
      l
      (begin (tputs (cursor-right))
             (shift-right l (next-char l)))))

(define (kill-line l k)
  (tputs (clr-eol))
  (make-line (line:left l) '()))