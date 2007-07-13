(define-record-type keychord
  (%make-keychord key control? meta?)
  keychord?
  (key      keychord:key)
  (control? keychord:control-down?)
  (meta?    keychord:meta-down?))

(define (make-keychord key . modifiers)
  (let-optionals modifiers ((control-down  #f)
                            (meta-down     #f))
    (if (not (char? key))
        (error "This is not a valid keychord:" key)
        (%make-keychord key control-down meta-down))))

(define (char->key c)
  (if (char-iso-control? c)
      (ascii->char (+ 96 (char->ascii c)))
      c))

(define (meta-pressed? n meta)
  (or meta
      (not (zero? (bitwise-and #x80 n)))))

(define (keychord c . args)
  (let-optionals args ((meta #f))
    (make-keychord (char->key c)
                   (char-iso-control? c)
                   (meta-pressed? (char->ascii c) meta))))

(define (keychord=? chord1 chord2)
  (if (not (and (keychord? chord1)
                (keychord? chord2)))
      (error "Invalid input")
      (and (char=? (keychord:key chord1)
                   (keychord:key chord2))
           (eq?    (keychord:control-down? chord1)
                   (keychord:control-down? chord2))
           (eq?    (keychord:meta-down? chord1)
                   (keychord:meta-down? chord2)))))

(define (keychord->string chord)
  (let ((output (open-output-string)))
    (if (keychord:control-down? chord)
        (display "C-" output))
    (if (keychord:meta-down? chord)
        (display "M-" output))
    (display (keychord:key chord) output)
    (get-output-string output)))


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
          (keychord (read-char) #t)
          (keychord c)))))
