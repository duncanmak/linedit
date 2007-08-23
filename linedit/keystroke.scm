;;; -*- Mode: Scheme; scheme48-package: keystroke -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; Keystroke abstraction
;;;
;;; The KEYSTROKE record represents a single keyboard input event.
;;;
;;; Keystrokes are described in an s-expression form:
;;;
;;; For single character input: (key #\a) or #\a
;;; For keystrokes that involve the control key: (control #\a)
;;; For keystrokes that involve the meta key: (meta #\a)
;;;
;;; Additionally, the follow symbols are recognized:
;;;
;;;  escape, return, backspace, left, right, up, down
;;;
;;; To create a KEYSTROKE structure, use PARSE-KEY to read in an
;;; s-expression describing the keystroke.
;;;

(define-record-type keystroke
  (%make-keystroke %value %meta?)
  keystroke?
  (%value char-value)
  (%meta? meta?))

(define-record-discloser keystroke
  (lambda (k)
    `(Keystroke ,(keystroke->sexpr k) ,(keystroke-hash k) )))

(define (parse-key form)
  (if (list? form)
      (let ((tag (car  form))
            (val (cadr form)))
        (case tag
          ((control) (%control  (parse-key val)))
          ((meta)    (%meta     (parse-key val)))
          ((key)     (%key     val))
          (else      (error "This is not a valid key form " tag))))
      (%key form)))

(define (keystroke-hash k)
  (cond
   ((not (keystroke? k))
    (error "this is not a keystroke " k))
   ((meta? k)
    (list 27 (char->ascii (char-value k))))      ; 27 is the ASCII code for escape
   ((list? (char-value k)) (char-value k))
   (else (char->ascii (char-value k)))))

;;; PRIVATE

(define (%key value)
  (cond
   ((symbol? value) (%make-keystroke (resolve-symbol value) #f))
   ((char? value) (%make-keystroke value #f))
   ((keystroke? value)  value)
   (else (error "input is invalid: " value))))

(define (%control value)
  (cond
   ((symbol? value) (%make-keystroke (resolve-symbol value) #f))
   ((char? value)
    (%make-keystroke (control-character value) #f))
   ((keystroke? value)
    (%make-keystroke (control-character (char-value value)) (meta? value)))
   (else (error "input is invalid: " value))))

(define (%meta value)
  (cond
   ((symbol? value)     (%make-keystroke (resolve-symbol value) #t))
   ((char? value)       (%make-keystroke value #t))
   ((keystroke? value)  (%make-keystroke (char-value value) #t))
   (else (error "input is invalid: " value))))

(define (keystroke->sexpr k)
  (let ((result (list (print k))))
    (cond
     ((and (meta? k) (control? k))
      (cons 'control (cons 'meta result)))
     ((meta?    k)   (cons 'meta    result))
     ((control? k)   (cons 'control result))
     (else           (cons 'key     result)))))

(define (print k)
  (let ((v (char-value k)))
    (cond
     ((special-name? v) => (lambda (s) s))
     ((char-iso-control? v)
      (ascii->char (+ (char->ascii v)
                      (- (char->ascii #\A) 1))))
     (else v))))

(define (control? k)
  (and (keystroke? k) (char-iso-control? (char-value k))))

(define (control-character c)
  (define (char-minus c1 c2)
    (ascii->char (+ 1 (- (char->ascii c1) (char->ascii c2)))))
  (cond
   ((char-iso-control? c) c)
   ((char-upper-case? c) (char-minus c #\A))
   ((char-lower-case? c) (char-minus c #\a))
   (else
    (case c
      ((#\@) (ascii->char   0))
      ((#\[) (ascii->char  27))
      ((#\\) (ascii->char  28))
      ((#\/) (ascii->char  29))
      ((#\^) (ascii->char  30))
      ((#\_) (ascii->char  31))
      ((#\?) (ascii->char 127))
      (else  (error "this cannot be a control character " c))))))

(define (resolve-symbol s)
  (case s
    ((escape)    (ascii->char  27))
    ((return)    (ascii->char  13))
    ((backspace) (ascii->char 127))
    ((left)      (map char->ascii (string->list (key-left))))
    ((right)     (map char->ascii (string->list (key-right))))
    ((up)        (map char->ascii (string->list (key-up))))
    ((down)      (map char->ascii (string->list (key-down))))
    (else        (error "this symbol is not recognized" s))))

(define (special-name? v)
  (cond
   ((char? v)
    (cond
     ((eq? v (ascii->char 27))  'escape)
     ((eq? v (ascii->char 13))  'return)
     ((eq? v (ascii->char 127)) 'backspace)
     (else #f)))
   ((list? v)
    (cond
     ((eq? v (key-left))  'left)
     ((eq? v (key-right)) 'right)
     ((eq? v (key-up))    'up)
     ((eq? v (key-down))  'down)
     (else #f)))
   (else #f)))