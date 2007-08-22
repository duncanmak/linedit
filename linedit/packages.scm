;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure commands commands-interface
  (open srfi-13 srfi-14 conditions helpers let-opt line keystroke keymap
        (modify terminfo (rename (tputs ti:tputs))) scheme-with-scsh signals tables)
  (files commands keybindings))

(define-structure helpers helpers-interface
  (open scheme-with-scsh tables keymap
        (modify terminfo (rename (tputs ti:tputs))) terminal-mode)
  (files utilities))

(define-structure keymap keymap-interface
  (open scheme-with-scsh keystroke let-opt tables)
  (files keymap))

(define-structure keystroke keystroke-interface
  (open scheme-with-scsh srfi-6 srfi-9 srfi-13 define-record-types table terminfo)
  (files keystroke))

(define-structure line line-interface
  (open srfi-6 srfi-9 srfi-13 scheme-with-scsh conditions
        define-record-types handle let-opt keymap keystroke terminal-mode)
  (files line))

(define-structure terminal-mode terminal-mode-interface
  (open scheme-with-scsh enumerated let-opt)
  (files terminal-mode))

(define-structure linedit linedit-interface
  (open scheme-with-scsh commands helpers line keymap keystroke terminal-mode tables terminfo)
  (files startup))