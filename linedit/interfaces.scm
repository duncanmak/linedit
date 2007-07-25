;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-interface commands-interface
  (export ((define-key) :syntax)
          insert-char
          delete-backward-char
          delete-char
          move-beginning-of-line
          move-end-of-line
          backward-char
          forward-char
          kill-line))

(define-interface keymap-interface
  (export lookup
          global-key-map
          meta-key-map))

(define-interface line-interface
  (export make-line
          line:left
          line:right
          line->string))

(define-interface keyboard-input-interface
  (export key
          process-input
          read-line))

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))


(define-interface linedit-interface
  (compound-interface commands-interface line-interface terminal-mode-interface keyboard-input-interface))