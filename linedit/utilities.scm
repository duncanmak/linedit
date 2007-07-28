(define (sane)
  (set-input-terminal-mode 'cooked))

(define (tty-info=? x y)
  (map (lambda (f) (= (f x) (f y)))
       (list tty-info:input-flags
             tty-info:output-flags
             tty-info:local-flags
             tty-info:control-flags
             tty-info:min
             tty-info:time)))

(define (print-table table)
  (table-walk
   (lambda (k v) (format #t "Key: ~A Value: ~A~%" k v))
   table))

(define (show-keybinding keyseq)
  (cond
   ((lookup-key global-keymap (kbd keyseq)) =>
    (lambda (command) (display command) (newline)))
   (else (format #t "~A is not bound to any command~%" keyseq))))

;; (define-syntax string-case
;;   (syntax-rules (=>)
;;     ((_ s ((key) (value)) ...)
;;      ()
;;      )))