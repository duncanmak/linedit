;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define global-keymap (make-integer-table))
(define meta-keymap   (make-integer-table))

(define (process key line . args)
  (let-optionals args ((keymap global-keymap))
    (cond
     ((lookup-key keymap key) =>
      (lambda (command)
        (command line key))))))

(define (lookup-key keymap key)
  (table-ref keymap (char->ascii key)))
