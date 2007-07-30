;;; -*- Mode: Scheme; scheme48-package: keyboard -*-

;;; predefined-keys
(define ff   (ascii->char  12))
(define cr   (ascii->char  13))
(define esc  (ascii->char  27))
(define del  (ascii->char 127))
(define bksp (ascii->char 223))

(define (kbd s)
  (cond
   ((char? s)
    (char->ascii s))
   ((= 1 (string-length s))
    (kbd (string-ref s 0)))
   (else
    (let* ((split     (infix-splitter "-"))
           (all-keys  (split s))
           (key       (car (reverse all-keys)))
           (modifiers (cdr (reverse all-keys))))
      (let loop ((result   '())
                 (keys     modifiers)
                 (add-key #t))
        (cond
         ((null? keys)
          (if add-key
              (append result (key->value key))
              result))
         ((member "M" keys)
          (loop (cons (char->ascii esc) result)
                (delete "M" keys)
                add-key))
         ((member "C" keys)
          (loop (cons (key->control-character key) result)
                (delete "C" keys)
                #f))))))))

(define (key->value k)
  (let ((len (string-length k)))
    (map
     char->ascii
     (cond
      ((= len 1) (list (string-ref k 0)))
      ((string=? k "<backspace>") (list del))
      ((string=? k "<left>") (string->list (key-sleft)))
      ((string=? k "<right>") (string->list (key-sright)))
      ((string=? k "RET") (list cr))
      ((string=? k "ESC") (list esc))
      (else (error "this is invalid " k))))))

(define (key->control-character k)
  (if (not (= 1 (string-length k)))
      (key->value k)
      (let ((c (string-ref k 0)))
        (- (char->ascii (char-downcase c)) (char->ascii #\`)))))
