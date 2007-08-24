;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define (readline . args)
  (let-optionals args ((prompt  "")
                       (history (disable-history)))
    (display prompt)
    (with-current-input-terminal-mode 'raw
      (let loop ((l  (make-empty-line prompt history))
                 (ch (read-char)))
        ((call-with-current-continuation
           (lambda (k)
             (lambda ()
               (with-handler (lambda (c next)
                               (k (lambda ()
                                    (if (interrupt? c)
                                        (line->string (car (condition-stuff c)))
                                        (next)))))
                 (lambda ()
                   (loop (process ch l)
                         (read-char))))))))))))

(define new-history  make-history)

(define disable-history empty-history)