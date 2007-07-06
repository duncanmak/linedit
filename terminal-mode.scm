;;; -*- Mode: Scheme; scheme48-package: terminal-mode -*-
;;;
;;; terminal-mode.scm: Terminal mode (based on the MIT Scheme API)
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define-enumeration terminal-mode (cooked raw))

(define (input-terminal-mode . args)
  (let-optionals args ((port (current-input-port)))
    (let* ((info      (tty-info port))
           (canonical (bitwise-and (tty-info:local-flags info)
                                   ttyl/canonical)))
      (if (zero? canonical)
          'raw
          'cooked))))

(define (set-input-terminal-mode mode . args)
  (let-optionals args ((port (current-input-port)))
    (case mode
      ((cooked) (set-tty-info/now port (input-terminal-cooked-mode)))
      ((raw)    (set-tty-info/now port (input-terminal-raw-mode)))
      (else     (error "This is not a valid terminal mode: " mode)))))

(define (call-with-input-terminal-mode mode thunk . args)
  (let-optionals args ((port (current-input-port)))
    (let ((orig (tty-info port)))
     (set-input-terminal-mode mode port)
     (thunk)
     (set-tty-info/flush port orig))))

(define-syntax with-input-terminal-mode
  (syntax-rules ()
    ((_ port mode body ...)
     (call-with-input-terminal-mode mode
         (lambda () body ...) port))))

(define-syntax with-current-input-terminal-mode
  (syntax-rules ()
    ((_ mode body ...)
     (call-with-input-terminal-mode mode
         (lambda () body ...)))))

(define $input-terminal-cooked-mode (make-fluid (tty-info (current-input-port))))

(define (input-terminal-cooked-mode)
  (fluid $input-terminal-cooked-mode))

(define (input-terminal-raw-mode)
  (let* ((info   (input-terminal-cooked-mode))
         (ispeed (tty-info:input-speed info))
         (ospeed (tty-info:output-speed info))
         (min    1)
         (time   0))
    (make-tty-info
     ;; c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
     (bitwise-not
      (bitwise-and ttyin/ignore-break
                   ttyin/interrupt-on-break
                   ttyin/mark-parity-errors
                   ttyin/7bits ttyin/nl->cr
                   ttyin/ignore-cr ttyin/cr->nl
                   ttyin/output-flow-ctl))
     ;; c_oflag &= ~OPOST;
     (bitwise-not ttyout/enable)

     ;; c_cflag &= ~(CSIZE|PARENB);
     (bitwise-not
      (bitwise-and ttyc/char-size
                   ttyc/enable-parity
                   ttyc/char-size8))

     ;; c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
     (bitwise-not
      (bitwise-and ttyl/echo ttyl/echo-nl
                   ttyl/canonical
                   ttyl/enable-signals
                   ttyl/extended))

     ispeed ospeed min time)))
