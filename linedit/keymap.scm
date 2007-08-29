;;; -*- Mode: Scheme; scheme48-package: keymap -*-
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; Keymap
;;;
;;; The linedit keymap is implemented using an integer table. The
;;; initial key to global-keymap must be an integer. In the simple
;;; case, the global-keymap maps an integer key to a Command; for
;;; more complicated key sequences, each integer key in the sequence
;;; is mapped to it own table, which ultimately must end in a Command.
;;;
;;; To facilitate dealing with the keymap, three operations are defined:
;;;
;;; DEFINE-KEY* keymap key command -> assigns command to key in keymap.
;;;
;;; LOOKUP-KEY key [keymap] -> looks up the value of key in keymap. If
;;; no keymap is supplied, the global-keymap is used.
;;;
;;; PROCESS key line [keymap] -> Uses LOOK-UP to find the appropriate
;;; Command and calls it with key and line.
;;;
;;; DEFINE-KEY is a macro over DEFINE-KEY*, so that keystroke
;;; description forms do not need to be explicitly quoted and parsed.
;;;

(define global-keymap (make-integer-table))

(define (lookup-key key . args)
  (let-optionals args ((keymap global-keymap))
    (cond
     ((integer? key) (table-ref keymap key))
     ((list? key)
      (if (null? (cdr key))
          (lookup-key (car key) keymap)
          (let ((value (lookup-key (car key) keymap)))
            (if (table? value)
                (lookup-key (cdr key) value)
                (error "this is not a valid key sequence" key))))))))

(define (process key line . args)
  (let-optionals args ((keymap global-keymap))
    (let ((value (lookup-key (char->ascii key) keymap)))
      (cond
       ((procedure? value) (value line key))
       ((table?     value) (process (read-char) line value))
       (else line)))))

(define-syntax define-key
  (syntax-rules ()
    ((_ keymap key-form value)
     (define-key* keymap (parse-key (quote key-form)) value))))

(define (define-key* keymap k v)
  (cond
   ((not (table? keymap))
    (error "keymap is invalid" keymap))
   ((not (keystroke? k))
    (error "this is not a keystroke"))
   (else
    (add-key keymap (keystroke-hash k) v))))

(define (add-key keymap k v)
  (cond
   ((integer? k)
    (table-set! keymap k v))
   ((list? k)
    (cond ((null? k))
          ((null? (cdr k)) (add-key keymap (car k) v))
          (else (let* ((key (car k))
                       (val (table-ref keymap key)))
                  (if (not val)
                      (begin (table-set! keymap key (make-integer-table))
                             (add-key (table-ref keymap key) (cdr k) v))
                      (add-key val (cdr k) v))))))
   (else (error "not a valid keystroke hash" k))))

