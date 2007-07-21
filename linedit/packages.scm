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
  (export ((define-keychord) :syntax)
          make-keychord
          get-keychord
          keychord->string
          string->keychord
          get-keychord
          special-keychord
          keychord?
          keychord=?
          keychord:key
          keychord:control-down?
          keychord:meta-down?))


(define-structure keychord keychord-interface
  (open srfi-1 srfi-6 srfi-9 srfi-13 formats let-opt scheme-with-scsh table)
  (files keychord keybinding))

(define-interface line-interface
  (export make-line
          read-line
          line->string))

(define-interface linedit-interface
  (compound-interface terminal-mode-interface keychord-interface line-interface))

(define-structure linedit linedit-interface
  (open srfi-1 srfi-6 srfi-9 srfi-13 formats scheme-with-scsh keychord terminfo terminal-mode)
  (files line))