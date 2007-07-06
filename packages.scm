;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))

(define-structure terminal-mode terminal-mode-interface
  (open enumerated fluids let-opt scheme-with-scsh)
  (files terminal-mode))