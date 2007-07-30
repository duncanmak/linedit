;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-interface commands-interface
  (export initialize-keymaps
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
  (export make-line
          line:left
          line:right
          line->port
          line->string
          string->line
          get-char
          shift-left
          shift-right
          beginning-of-line
          end-of-line
          line-length
          line-insert
          line?
          process-line))

(define-interface keyboard-interface
  (export kbd
          ff cr esc del bksp))

(define-interface keymap-interface
  (export define-key
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
  (export print-table sane set-output! show-keybinding tputs))

(define-interface linedit-interface
  (compound-interface commands-interface
                      line-interface
                      keyboard-interface
                      keymap-interface
                      helpers-interface
                      terminal-mode-interface))