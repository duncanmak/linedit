;;; -*- Mode: Scheme; scheme48-package: echo -*-

(define-structure echo (export echo)
  (open formats scheme helpers linedit)
  (begin

    (define-key global-keymap (control #\h) (lambda (l k) (print-table global-keymap) l))

    (define (echo)
      (let ((h (new-history)))
        (let loop ((input (readline "Say what? " h)))
          (if (string=? "bye" input)
             (format #t "ciao!~%")
             (loop (readline "Again? " h))))))))