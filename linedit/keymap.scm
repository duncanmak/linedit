;;; -*- Mode: Scheme; scheme48-package: keymap -*-

(define global-keymap (make-integer-table))

(define (lookup-key key . args)
  (let-optionals args ((keymap global-keymap))
    (cond
     ((number? key) (table-ref keymap key))
     ((list? key)
      (if (null? (cdr key))
          (lookup-key (car key) keymap)
          (let ((value (lookup-key (car key) keymap)))
            (if (table? value)
                (lookup-key (cdr key) value)
                (error "this is not a valid key sequence" key))))))))

(define (process key line . args)
  (let-optionals args ((keymap global-keymap))
    (let ((value (lookup-key (char->ascii key) keymap)))
      (cond
       ((procedure? value) (value line key))
       ((table?     value) (process (read-char) line value))
       (else line)))))

(define (define-key keymap k v)
  (cond
   ((not (table? keymap))
    (error "keymap is invalid"))
   ((number? k)
    (table-set! keymap k v))
   ((list? k)
    (if (null? (cdr k))
        (define-key keymap (car k) v)
        (let* ((key (car k))
               (val (table-ref keymap key)))
          (if (not val)
              (begin (table-set! keymap key (make-integer-table))
                     (define-key (table-ref keymap key) (cdr k) v))
              (define-key val (cdr k) v)))))
   (else (error "key must be either be a number or a list of numbers" k))))

