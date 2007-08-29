;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (readline . args)
  (let-optionals args ((prompt     "")
                       (history    (disable-history))
                       (input-port (current-input-port)))
    (if (not (string-null? prompt)) (display prompt))
    (call-with-current-continuation
      (lambda (return)
        (with-current-input-port input-port
            (with-current-input-terminal-mode 'raw
              (let loop ((l (make-empty-line prompt history)))
                (loop
                 (with-handler (lambda (c next)
                                 (if (interrupt? c)
                                     (return (line->string (car (condition-stuff c))))
                                     (next)))
                   (lambda ()
                     (loop (process (read-char) l))))))))))))

(define new-history  make-history)
(define disable-history empty-history)