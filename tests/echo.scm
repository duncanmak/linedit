(define-structure echo (export echo)
  (open scheme linedit)
  (begin
    (define (echo)
     (let ((line (readline "Say what? ")))
       line))))