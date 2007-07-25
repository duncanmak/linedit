;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define global-key-map (make-integer-table))
(define meta-key-map   (make-integer-table))

(define (lookup keymap key line)
  (cond
   ((table-ref keymap (char->ascii key)) =>
    (lambda (command) (command key line)))
   (else (insert-char key line))))
