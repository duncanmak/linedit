;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (insert-char k l)
  (tputs (enter-insert-mode))
  (display k)
  (tputs (exit-insert-mode))
  (make-line (cons k (line:left l))
             (line:right l)))

(define (delete-backward-char k l)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (tputs (delete-character))
             (make-line (cdr (line:left l))
                        (line:right l)))))

(define (delete-char k l)
  (if (null? (line:right l))
      l
      (begin (tputs (delete-character))
             (make-line (line:left l)
                        (cdr (line:right l))))))

(define (move-beginning-of-line k l)
  (tputs (column-address 0))
  (make-line '() (reverse (line:left l))))

(define (move-end-of-line k l)
  (let ((len (+ 1 (length (line:left  l))
                (length (line:right l)))))
    (tputs (column-address len)))
  (make-line (append (reverse (line:right l)) (line:left l)) '()))

(define (backward-char k l)
  (if (null? (line:left l))
      l
      (let ((char (car (line:left l))))
        (tputs (cursor-left))
        (make-line (cdr (line:left l))
                   (cons char (line:right l))))))

(define (forward-char k l)
  (if (null? (line:right l))
      l
      (let ((char (car (line:right l))))
        (tputs (cursor-right))
        (make-line (cons char (line:left l))
                   (cdr (line:right l))))))

(define (kill-line k l)
  (tputs (clr-eol))
  (make-line (line:left l) '()))