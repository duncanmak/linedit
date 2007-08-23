(define-structure echo (export echo)
  (open formats scheme linedit)
  (begin
    (define (echo)
      (let loop ((input (readline "Say what? ")))
        (if (string=? "bye" input)
            (format #t "ciao!~%")
            (loop (readline "Again? ")))))))