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

(define (line->string l)
  (cond
   ((not (line? l))
    "Not a line")
   ((and (null? (line:left l))
         (null? (line:right l)))
    "Empty line")
   (else
    (string-append (reverse-list->string (line:left l))
                   (list->string (line:right l))))))

(define (process-line . args)
  (let-optionals args ((buf  (make-line)))
    (with-current-input-terminal-mode 'raw
      (let loop ((char (read-char))
                 (line buf))
        (let ((result (process char line)))
          (if (char=? char cr)
              (line->string result)
              (loop (read-char)
                    result)))))))