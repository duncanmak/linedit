;;; -*- Mode: Scheme; scheme48-package: line -*-

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
                   "^"
                   (list->string (line:right l))))))

(define (string->line s)
  (if (not (string-contains s "^"))
      (make-line (reverse (string->list s)))
   (let* ((split (infix-splitter "^"))
          (line  (split s))
          (left  (car  line))
          (right (cadr line)))
     (make-line (reverse (string->list left))
                (string->list right)))))

(define (get-char l direction)
  (if (null? (direction l))
      '()
      (car (direction l))))

(define (line-insert l char)
  (make-line (cons char (line:left l))
             (line:right l)))

(define (shift-left l . chars)
  (make-line (cdr (line:left l))
             (append chars (line:right l))))

(define (shift-right l . char)
  (make-line (append char (line:left l))
             (cdr (line:right l))))

(define (beginning-of-line l)
  (make-line '() (append (reverse (line:left l))
                         (line:right l))))

(define (end-of-line l)
  (make-line (append (reverse (line:right l))
                     (line:left l)) '()))

(define (line-length l)
  (+ (length (line:left  l))
     (length (line:right l))))

(define (process-line line)
  (with-current-input-terminal-mode 'raw
    (let loop ((c (read-char))
               (l line))
      (let ((result (process c l)))
        (if (char=? c cr)
            result
            (loop (read-char)
                  result))))))