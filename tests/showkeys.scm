(define-structure showkeys (export show-keys)
  (open scheme ascii formats terminal-mode)
  (begin
    (define (show-keys)
     (with-current-input-terminal-mode 'raw
       (let loop ()
         (let* ((c  (read-char)))
           (if (char=? c #\q)
               (begin (newline) (display "bye bye now") (newline))
               (begin (format #t "~A " (char->ascii c))
                      (loop)))))))))