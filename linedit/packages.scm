;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-structure commands commands-interface
  (open srfi-13 keymap keyboard-input line terminfo scheme-with-scsh tables)
  (files commands keybindings))

(define-structure line line-interface
  (open srfi-1 srfi-9 srfi-13 let-opt scheme-with-scsh terminal-mode)
  (files line))

(define-structure keyboard-input keyboard-input-interface
  (open srfi-1 srfi-9 srfi-13 let-opt keymap scheme-with-scsh table terminal-mode)
  (files keyboard line))

(define-structure keymap keymap-interface
  (open line scheme-with-scsh tables)
  (files keymap))

(define-structure terminal-mode terminal-mode-interface
  (open scheme-with-scsh enumerated let-opt)
  (files terminal-mode))

(define-structure linedit linedit-interface
  (open scheme-with-scsh commands line keyboard-input keymap terminal-mode))