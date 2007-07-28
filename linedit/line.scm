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

(define (prev-char l)
  (car (line:left l)))

(define (next-char l)
  (car (line:right l)))

(define (line-insert l char)
  (make-line (cons char (line:left l))
             (line:right l)))

(define (line-remove l char)
  (make-line (cdr (line:left l))
             (line:right l)))

(define (shift-left l . chars)
  (make-line (cdr (line:left l))
             (append chars (line:right l))))

(define (shift-right l . chars)
  (make-line (append chars (line:left l))
             (cdr (line:right l))))

(define (beginning-of-line l)
  (make-line '() (reverse (line:left l))))

(define (end-of-line l)
  (make-line (append (reverse (line:right l)) (line:left l)) '()))

(define (line-length l)
  (+ (length (line:left  l))
     (length (line:right l))))

(define (process-line line)
  (with-current-input-terminal-mode 'raw
    (let loop ((char (read-char))
               (line line))
      (let ((result (process char line)))
        (if (char=? char cr)
            result
            (loop (read-char)
                  result))))))