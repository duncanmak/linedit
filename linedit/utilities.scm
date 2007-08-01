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
   table)
  table)

(define (show-keybinding keyseq)
  (cond
   ((table-ref global-keymap (kbd keyseq)) =>
    (lambda (command) (display command) (newline)))
   (else (format #t "~A is not bound to any command~%" keyseq))))

(define show-output #t)
(define (set-output! flag) (set! show-output flag))

(define (tputs . s)
  (if show-output
      (for-each (lambda (i) (ti:tputs i)) s)))