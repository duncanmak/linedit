;;; -*- Mode: Scheme; scheme48-package: commands -*-
;;;
;;; Copyright � 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;
;;; This code is placed in the Public Domain.  All warranties are
;;; disclaimed.
;;;
;;; Linedit Commands
;;;
;;; This file contains the commands used by linedit.
;;;
;;; All commands take the form
;;;
;;;        command line [key]
;;;
;;; and return a new line.
;;;
;;; While the 'key' argument is optional, it cannot be omitted
;;; entirely. Commands that do not make use of the key argument should
;;; be defined in this form:
;;;
;;;        (define (command-that-discards-key line . key))
;;;
;;; This allows callers of the command to pass only the 'line'
;;; argument, but preserves the command interface.
;;;

(define (accept-line l . k)
  (newline)
  (add-line-history (line:history l) (line->string l))
  (reset-edit-history (line:history l))
  (signal 'interrupt l))

(define (insert-char l k)
  (tputs (enter-insert-mode)
         (string k)
         (exit-insert-mode))
  (line-insert l k))

;; (define (insert-char l k)
;;   (let ((nl (line-insert l k)))
;;     (display-line nl)
;;     nl))

(define (delete-backward-char l . k)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left)
                    (delete-character))
             (shift-left l))))

(define (delete-char l . k)
  (if (null? (line:right l))
      l
      (begin (tputs (delete-character))
             (shift-right l))))

(define (move-beginning-of-line l . k)
  (tputs (column-address (line:column l)))
  (beginning-of-line l))

(define (move-end-of-line l . k)
  (tputs (column-address (+ (line:column l) (line:length l))))
  (end-of-line l))

(define (backward-char l . k)
  (if (null? (line:left l))
      l
      (begin (tputs (cursor-left))
             (shift-left l (get-char l line:left)))))

(define (forward-char l . k)
  (if (null? (line:right l))
      l
      (begin (tputs (cursor-right))
             (shift-right l (get-char l line:right)))))

(define (clear-line l . k)
  (tputs (column-address (line:column l))
         (clr-eol))
  l)

(define (display-line l . k)
  (tputs (column-address 0)
         (line:prompt l)
         (line->string l)
         (column-address (line:cursor l)))
  l)

(define (kill-line l . k)
  (tputs (clr-eol))
  (copy-line l (line:left l) '()))

(define (move-word l direction action)
  (let loop ((line l)
             (skip-space #t))
    (let ((c (get-char line direction)))
      (cond
       ((null? c) line)
       ((char-letter? c) (loop (action line) #f))
       ((and (not (char-alphabetic? c)) skip-space)
        (loop (action line) #t))
       (else line)))))

(define (backward-word l . k)
  (move-word l line:left backward-char))

(define (forward-word l . k)
  (move-word l line:right forward-char))

(define (kill-word l . k)
  (move-word l line:right delete-char))

(define (backward-kill-word l . k)
  (move-word l line:left delete-backward-char))

(define (history-next-input l . k)
  (let* ((h    (line:history l))
         (hist (get-line-history h 'next)))
    (if (not (null? hist))
        (replace-with-string l hist)
        l)))

(define (history-prev-input l . k)
  (let* ((h    (line:history l))
         (hist (get-line-history h 'previous)))
    (if (not (null? hist))
        (replace-with-string l hist)
        l)))

(define (replace-with-string l s)
  (clear-line l)
  (display s)
  (copy-line l (reverse (string->list s))))

(define (undo l . k)
  (let* ((h    (line:history l))
         (edit (get-edit-history h 'previous)))
    (if (not (null? edit))
        (replace-with-line l edit)
        l)))

(define (redo l . k)
  (let* ((h    (line:history l))
         (edit (get-edit-history h 'next)))
    (if (not (null? edit))
        (replace-with-line l edit)
        l)))

(define (replace-with-line l nl)
  (clear-line l)
  (display-line nl)
  nl)