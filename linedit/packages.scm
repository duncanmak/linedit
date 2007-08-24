;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure commands commands-interface
  (open scheme ascii char-predicates-lib formats srfi-13 srfi-14  conditions helpers history let-opt line keystroke keymap
        (modify terminfo (rename (tputs ti:tputs))) signals tables)
  (files commands keybindings))

(define-structure helpers helpers-interface
  (open scheme formats tables keymap
        (modify terminfo (rename (tputs ti:tputs))) terminal-mode)
  (files utilities))

(define-structure history history-interface
  (open scheme let-opt srfi-9 srfi-13 define-record-types methods ring-buffer signals)
  (files history))

(define-structure keymap keymap-interface
  (open scheme ascii error-package keystroke let-opt tables)
  (files keymap))

(define-structure keystroke keystroke-interface
  (open scheme ascii char-predicates-lib srfi-6 srfi-9 srfi-13 define-record-types error-package table terminfo)
  (files keystroke))

(define-structure line line-interface
  (open scheme srfi-6 srfi-9 srfi-13
        define-record-types field-reader-package history let-opt keymap keystroke)
  (files line))

(define-structure ring-buffer ring-buffer-interface
  (open scheme srfi-9 define-record-types let-opt util)
  (files ring-buffer))

(define-structure terminal-mode terminal-mode-interface
  (open scheme scsh-level-0 enumerated let-opt)
  (files terminal-mode))

(define-structure linedit linedit-interface
  (open scheme conditions commands handle history let-opt line keymap terminal-mode)
  (files linedit startup))