;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))

(define-structure terminal-mode terminal-mode-interface
  (open scheme-with-scsh enumerated fluids let-opt)
  (files terminal-mode))

(define-interface keychord-interface
  (export make-keychord
          get-keychord
          keychord->string
          string->keychord
          keychord
          keychord?
          keychord=?
          keychord:key
          keychord:control-down?
          keychord:meta-down?))

(define-structure keychord keychord-interface
  (open srfi-1 srfi-6 srfi-9 srfi-13 formats let-opt scheme-with-scsh)
  (files keychord))