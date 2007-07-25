;;; -*- Mode: Scheme; scheme48-package: linedit -*-

;;; predefined-keys
(define bel  (ascii->char   7))
(define bs   (ascii->char   8))
(define ht   (ascii->char   9))
(define vt   (ascii->char  11))
(define ff   (ascii->char  12))
(define cr   (ascii->char  13))
(define esc  (ascii->char  27))
(define del  (ascii->char 127))
(define bksp (ascii->char 223))

(define (char->name c)
  (cond
   ((char-letter+digit? c) c)
   ((char-special?      c) => (lambda (name) name))
   ((char-iso-control?  c) (ascii->char (+ 96 (char->ascii c))))
   (else (error "unknown char" c))))

(define (char-special? c)
  (case c
    ((#\newline) "RET")
    ((bksp)       "<backspace>")
    ((del)       "<delete>")
    ((esc)       "ESC")
    (else #f)))

(define (key s)
  (cond
    ((string=? s "<backspace>") bksp)
    ((string=? s "<delete>") del)
    ((string=? s "RET") #\newline)
    ((string=? s "ESC") esc)
    (else
     (let* ((len  (string-length s))
            (idx  (string-index-right s #\-))
            (key  (if idx (substring s (+ 1 idx) len) s))
            (mods (if idx (substring s 0 idx) ""))
            (c    (string-ref key 0)))
       (if (string-contains mods "C")
           (ascii->char (- (char->ascii (char-downcase c)) 96))
           c)))))

(define (process-input key . args)
  (let-optionals args ((line (make-line)))
    (let ((line (lookup global-key-map key line)))
      line)))


