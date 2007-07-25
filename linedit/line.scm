;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-record-type line
  (%make-line left right)
  line?
  (left   line:left  line:set-left!)
  (right  line:right line:set-right!))

(define (make-line . args)
  (let-optionals args ((left  '())
                       (right '()))
    (%make-line left right)))

(define (read-line . args)
  (let-optionals args ((line (make-line))
                       (port (current-input-port)))
    (with-current-input-terminal-mode 'raw
      (let loop ((key  (read-char port))
                 (line (process-input key line)))
        (if (char=? key #\newline)
            line
            (loop (read-char port)
                  (process-input key line)))))))