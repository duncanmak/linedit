;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (readline . args)
  (let-optionals args ((prompt     "")
                       (input-port (current-input-port))
                       (history    (disable-history)))
    (if (not (string-null? prompt)) (display prompt))
    (call-with-current-continuation
      (lambda (return)
        (with-input-terminal-mode input-port 'raw
          (let loop ((l (make-empty-line prompt history)))
            (loop
             (with-handler (lambda (c next)
                             (if (interrupt? c)
                                 (return (line->string (car (condition-stuff c))))
                                 (next)))
               (lambda ()
                 (loop (process (read-char input-port) l)))))))))))

(define new-history  make-history)

(define disable-history empty-history)