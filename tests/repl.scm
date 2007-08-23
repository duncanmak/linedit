(define-structure repl (export repl quit)
  (open scheme srfi-6 srfi-13 linedit)
  (begin
    (define continue #t)

    (define (quit . args)
      (set! continue #f))

    (define (evaluate env)
      (let* ((input (readline ">> "))
             (port  (string->port input))
             (value (if (null? port) "" (eval (read port) env))))
        (display value)
        (newline)))

    (define (string->port s)
      (if (string-null? s)
          '()
          (open-input-string s)))

    (define (repl)
      (define-key global-keymap (control #\q) quit)
      (display "Welcome to Duncan's enhanced REPL") (newline)
      (display "To quit, press C-q RET") (newline)
      (let loop ()
        (if continue
            (let ((env (scheme-report-environment 5)))
              (evaluate env)
              (loop))
            (begin (display "Bye!")
                   (set! continue #t)
                   (newline)))))))