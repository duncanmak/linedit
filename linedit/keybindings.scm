;;; -*- Mode: Scheme; scheme48-package: commands -*-
(define (initialize-keymap . args)
  (setup-terminal)
  (define-key global-keymap (control #\a) move-beginning-of-line)
  (define-key global-keymap (control #\e) move-end-of-line)
  (define-key global-keymap (control #\f) forward-char)
  (define-key global-keymap (control #\b) backward-char)
  (define-key global-keymap (key 'right) forward-char)
  (define-key global-keymap (key 'left)  backward-char)
  (define-key global-keymap (control #\k) kill-line)
  (define-key global-keymap (key 'backspace) delete-backward-char)
  (define-key global-keymap (control #\d) delete-char)
  (define-key global-keymap (key 'return) show-newline)

  (define-key global-keymap (meta #\d) kill-word)
  (define-key global-keymap (meta 'backspace) backward-kill-word)
  (define-key global-keymap (meta #\f) forward-word)
  (define-key global-keymap (meta #\b) backward-word)

  (let-optionals args ((keymaps (list global-keymap)))
    (for-each
     (lambda (keymap)
       (char-set-for-each
        (lambda (c) (define-key keymap (key c) insert-char))
        (char-set-union char-set:graphic
                        char-set:blank)))
     keymaps)))
