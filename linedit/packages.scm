;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure commands commands-interface
  (open srfi-13 srfi-14 helpers let-opt line keyboard keymap
        (modify terminfo (rename (tputs ti:tputs))) scheme-with-scsh tables)
  (files commands keybindings))

(define-structure helpers helpers-interface
  (open scheme-with-scsh tables keyboard keymap
        (modify terminfo (rename (tputs ti:tputs))) terminal-mode)
  (files utilities))

(define-structure keyboard keyboard-interface
  (open scheme-with-scsh srfi-1 srfi-9 srfi-13 let-opt keymap table terminfo terminal-mode)
  (files keyboard))

(define-structure keymap keymap-interface
  (open scheme-with-scsh let-opt tables)
  (files keymap))

(define-structure line line-interface
  (open srfi-6 srfi-9 srfi-13 scheme-with-scsh define-record-types let-opt keyboard keymap terminal-mode)
  (files line))

(define-structure terminal-mode terminal-mode-interface
  (open scheme-with-scsh enumerated let-opt)
  (files terminal-mode))

(define-structure linedit linedit-interface
  (open scheme-with-scsh commands helpers line keymap keyboard terminal-mode tables terminfo))