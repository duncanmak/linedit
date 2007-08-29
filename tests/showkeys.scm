(define-structure showkeys (export show-keys show-keyseq)
  (open scheme ascii formats srfi-13 terminal-mode)
  (begin
    (define (show-keys)
     (with-current-input-terminal-mode 'raw
       (let loop ()
         (let* ((c  (read-char)))
           (if (char=? c #\q)
               (begin (newline) (display "bye bye now") (newline))
               (begin (format #t "~A " (char->ascii c))
                      (loop)))))))
    (define (show-keyseq s)
      (string-for-each (lambda (c) (format #t "~a " (char->ascii c))) s) )
    ))