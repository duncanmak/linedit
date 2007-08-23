;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface commands-interface
  (export initialize-keymap
          insert-char
          delete-backward-char
          delete-char
          move-beginning-of-line
          move-end-of-line
          backward-char
          backward-word
          forward-char
          forward-word
          kill-line))

(define-interface line-interface
  (export make-empty-line
          copy-line
          line:left
          line:right
          line:column
          line:length
          line->port
          line->string
          string->line
          get-char
          shift-left
          shift-right
          beginning-of-line
          end-of-line
          line-insert
          line?
          readline))

(define-interface keystroke-interface
  (export parse-key
          keystroke-hash
          keystroke?))

(define-interface keymap-interface
  (export ((define-key) :syntax)
          define-key*
          lookup-key
          global-keymap
          process))

(define-interface terminal-mode-interface
  (export ((with-current-input-terminal-mode) :syntax)
          ((with-input-terminal-mode) :syntax)
          call-with-input-terminal-mode
          input-terminal-mode
          set-input-terminal-mode))

(define-interface helpers-interface
    (export 1+ 1- print-table sane tputs))


(define-interface linedit-interface
  (export readline
          initialize-keymap
          define-key
          global-keymap))
