;;; -*- Mode: Scheme; scheme48-package: keyboard -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; Keyboard abstraction
;;;
;;; The KBD function is a convenient way to denote a key sequence as a string:
;;;
;;; kbd s => list of integers representing the key sequence
;;;
;;; For example, (kbd "a") => '(97)
;;;              (kbd "C-a") => '(1)
;;;              (kbd "M-a") => '(27 97)
;;;              (kbd "M-C-a") => '(27 1)
;;;
;;; Additionally, kbd recognizes the following keys
;;;
;;;               ESC, RET, <backspace>, <left>, <right>
;;; 

;;; predefined characters not available in #\form
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
         ((member "C" keys)
          (loop (cons (key->control-character key) result)
                (delete "C" keys)
                #f))
         ((member "M" keys)
          (loop (cons (char->ascii esc) result)
                (delete "M" keys)
                add-key))))))))

(define (key->value k)
  (map
   char->ascii
   (cond
    ((= 1 (string-length k)) (list (string-ref k 0)))
    ((string=? k "<backspace>") (list del))
    ((string=? k "<left>") (string->list (key-left)))
    ((string=? k "<right>") (string->list (key-right)))
    ((string=? k "RET") (list cr))
    ((string=? k "ESC") (list esc))
    (else (error "this is invalid " k)))))

(define (key->control-character k)
  (if (not (= 1 (string-length k)))
      (key->value k)
      (let ((c (string-ref k 0)))
        (- (char->ascii (char-downcase c)) (char->ascii #\`)))))
