(define (1+ x) (+ x 1))
(define (1- x) (- x 1))

(define (sane)
  (set-input-terminal-mode 'cooked))

(define (print-table table)
  (table-walk
   (lambda (k v) (format #t "Key: ~A Value: ~A~%" k v))
   table)
  table)

(define (tputs . s) (for-each (lambda (i) (ti:tputs i)) s))