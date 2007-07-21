;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-record-type line
  (%make-line left right)
  line?
  (left   line:left  line:set-left!)
  (right  line:right line:set-right!))

(define (make-line) (%make-line '() '()))

(define (read-line line)
  (with-current-input-terminal-mode 'raw
      (let loop ()
        (let ((chord (get-keychord)))
          (cond
           ((keychord=? chord (special-keychord 'cr))
            (newline))
           ((keychord=? chord (special-keychord 'bksp))
            (begin (line:remove-char line)
                   (loop)))
           ((keychord=? chord (string->keychord "C-a"))
            (begin (line:move-beginning-of-line line)
                   (loop)))
           (else (begin (line:insert-char line (keychord:key chord))
                        (loop))))))))

(define (line->string line)
  (if (line? line)
      (string-append (reverse-list->string (line:left line))
                     (list->string (line:right line)))))

(define (line:insert-char line key)
  (display key)
  (line:set-left! line (cons key (line:left line))))

(define (line:remove-char line)
  (tputs (cursor-left))
  (tputs (delete-character))
  (line:set-left! line (cdr (line:left line))))

(define (line:move-beginning-of-line line)
  (tputs (clr-bol))
  (let loop ((l (reverse (line:left line))))
    (if (not (null? l))
        (begin (line:set-right! line (cons (car l) (line:right line)))
               (loop (cdr l))))))