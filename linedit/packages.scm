;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-structure commands commands-interface
  (open srfi-13 srfi-14 line keyboard keymap terminfo scheme-with-scsh tables)
  (files commands keybindings))

(define-structure line line-interface
  (open srfi-9 srfi-13 scheme-with-scsh let-opt keyboard keymap terminal-mode)
  (files line))

(define-structure keyboard keyboard-interface
  (open scheme-with-scsh srfi-9 srfi-13 let-opt keymap table terminal-mode)
  (files keyboard line))

(define-structure keymap keymap-interface
  (open scheme-with-scsh let-opt tables)
  (files keymap))

(define-structure terminal-mode terminal-mode-interface
  (open scheme-with-scsh enumerated let-opt)
  (files terminal-mode))

(define-structure linedit linedit-interface
  (open scheme-with-scsh commands line keymap keyboard terminal-mode tables terminfo)
  (files utilities))