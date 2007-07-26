;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-interface commands-interface
  (export ((define-key) :syntax)
          initialize-keymaps
          insert-char
          delete-backward-char
          delete-char
          move-beginning-of-line
          move-end-of-line
          backward-char
          forward-char
          kill-line))

(define-interface line-interface
  (export make-line
          line:left
          line:right
          line->string
          line?))

(define-interface keyboard-interface
  (export kbd
          ff cr esc del bksp
          process-line))

(define-interface keymap-interface
  (export global-keymap
          meta-keymap
          process))

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))

(define-interface misc-interface
  (export sane print-table))

(define-interface linedit-interface
  (compound-interface commands-interface
                      line-interface
                      keyboard-interface
                      keymap-interface
                      misc-interface
                      terminal-mode-interface))