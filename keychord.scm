;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-record-type keychord
  (%make-keychord key control? meta?)
  keychord?
  (key      keychord:key)
  (control? keychord:control-down?)
  (meta?    keychord:meta-down?))

(define (make-keychord key . modifiers)
  (let-optionals modifiers ((control-down  (char-iso-control? key))
                            (meta-down     #f))
    (%make-keychord (key->char key) control-down meta-down)))

(define (key->char c)
  (if (char-iso-control? c)
      (ascii->char (+ 96 (char->ascii c)))
      c))

(define (meta-pressed? n meta)
  (or meta
      (not (zero? (bitwise-and #x80 n)))))

(define (keychord=? chord1 chord2)
  (if (not (and (keychord? chord1)
                (keychord? chord2)))
      (error "Not chords")
      (and (char=? (keychord:key chord1)
                   (keychord:key chord2))
           (eq?    (keychord:control-down? chord1)
                   (keychord:control-down? chord2))
           (eq?    (keychord:meta-down? chord1)
                   (keychord:meta-down? chord2)))))

(define (keychord-hash chord)
  (* (char->ascii (keychord:key chord))
     (if keychord:control-down? 1 255)
     (if keychord:meta-down? 1 255)))

(define (special-keychord name)
  (case name
    ((lf)     (make-keychord (ascii->char 10)))
    ((cr)     (make-keychord (ascii->char 13)))
    ((space)  (make-keychord (ascii->char 32)))
    ((bksp)   (make-keychord (ascii->char 223) #t))))

(define (special-keychord->string chord)
  (cond
   ((or (keychord=? chord (special-keychord 'lf))
        (keychord=? chord (special-keychord 'cr)))
    "RET")
   ((keychord=? chord (special-keychord 'bksp)) "<backspace>")
   ((keychord=? chord (special-keychord 'space)) "SPC")
   (else #f)))

(define (keychord->string chord)
  (let ((output (open-output-string))
        (key    (keychord:key chord)))
     (or (special-keychord->string chord)
         (begin
           (if (keychord:control-down? chord)
               (display "C-" output))
           (if (keychord:meta-down? chord)
               (display "M-" output))
           (format output "~A (~A)" key (char->ascii key))
           (get-output-string output)))))

(define (string->keychord s)
  (let* ((len  (string-length s))
         (key  (string-ref s (- len 1)))
         (mods (substring s 0 (- len 2))))
    (make-keychord key
                   (string-contains mods "C")
                   (string-contains mods "M"))))

(define (get-keychord . args)
  (let-optionals args ((port (current-input-port)))
    (let ((c (read-char port)))
      (if (char=? (ascii->char 27) c)
          (make-keychord (read-char) (char-iso-control? c) #t)
          (make-keychord c)))))
