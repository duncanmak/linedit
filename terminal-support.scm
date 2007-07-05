;;; -*- Mode: Scheme; scheme48-package: terminfo -*-
;;;
;;; terminal-support.scm: Terminal support
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

;;; see cfmakeraw()
(define (make-raw-tty-info tty)
  (let* ((info          (tty-info tty))
         (raw           (copy-tty-info info))
         (input-flags   (tty-info:input-flags info))
         (output-flags  (tty-info:output-flags info))
         (control-flags (tty-info:control-flags info))
         (local-flags   (tty-info:local-flags info)))
    ; c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP|INLCR|IGNCR|ICRNL|IXON);
    (set-tty-info:input-flags
     raw (bitwise-and
          input-flags
          (bitwise-not
           (bitwise-ior ttyin/ignore-break
                        ttyin/interrupt-on-break
                        ttyin/mark-parity-errors
                        ttyin/7bits ttyin/nl->cr
                        ttyin/ignore-cr ttyin/cr->nl
                        ttyin/output-flow-ctl))))
    ; c_oflag &= ~OPOST;
    (set-tty-info:output-flags
     raw (bitwise-and
          output-flags (bitwise-not ttyout/enable)))
    ; c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
    (set-tty-info:local-flags
     raw (bitwise-and
          local-flags
          (bitwise-not
           (bitwise-ior ttyl/echo ttyl/echo-nl
                        ttyl/canonical
                        ttyl/enable-signals
                        ttyl/extended))))
    ; c_cflag &= ~(CSIZE|PARENB);
    (set-tty-info:control-flags
     raw (bitwise-and
          control-flags
          (bitwise-not
           (bitwise-ior ttyc/char-size
                        ttyc/enable-parity))))
    ; c_cflag |= CS8;
    (set-tty-info:control-flags
     raw (bitwise-ior control-flags ttyc/char-size8))
    raw))