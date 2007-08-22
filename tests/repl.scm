(define-structure repl (export repl quit)
  (open scheme srfi-6 linedit)
  (begin
    (define continue #t)

    (define (quit . args)
      (set! continue #f))

    (define (evaluate env)
      (let* ((line  (process-line (make-line)))
             (input (line->port line))
             (value (eval (read input) env)))
        (display value)
        (newline)))

    (define (repl)
      (define-key global-keymap (control #\x) quit)
      (let loop ()
        (if continue
            (let ((env (scheme-report-environment 5)))
              (evaluate env)
              (loop))
            (begin (display "Bye bye now")
                   (newline)))))))