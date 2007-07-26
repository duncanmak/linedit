;;; -*- Mode: Scheme; scheme48-package: linedit -*-

;;; predefined-keys
(define ff   (ascii->char  12))
(define cr   (ascii->char  13))
(define esc  (ascii->char  27))
(define del  (ascii->char 127))
(define bksp (ascii->char 223))

(define (kbd s)
  (cond
    ((string=? s "<backspace>") del)
    ((string=? s "RET") cr)
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

