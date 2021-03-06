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
          history-next-input
          history-prev-input
          undo redo
          accept-line
          kill-line
          clear-line
          display-line))

(define-interface history-interface
  (export add-edit-history
          add-line-history
          history:edit
          history:line
          get-edit-history
          get-line-history
          reset-edit-history
          make-history
          empty-history
          max-history
          max-undo
          keep-duplicates
          keep-blanks))

(define-interface line-interface
  (export make-empty-line
          copy-line
          line:left
          line:right
          line:column
          line:cursor
          line:length
          line:history
          line:prompt
          line->string
          string->line
          get-char
          shift-left
          shift-right
          beginning-of-line
          end-of-line
          line-insert
          line?))

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

(define-interface ring-buffer-interface
  (export make-ring-buffer
          ring-buffer?
          ring-buffer:empty?
          ring-buffer:add
          ring-buffer:length
          ring-buffer:next
          ring-buffer:previous
          ring-buffer:peek
          ring-buffer:reset!))

(define-interface linedit-interface
  (export readline
          new-history
          disable-history
          initialize-keymap
          define-key
          global-keymap))
