;;; -*- Mode: Scheme; scheme48-package: commands -*-
(define (initialize-keymaps . args)
  (setup-terminal)
  (let-optionals args ((keymaps (list global-keymap)))
    (for-each
     (lambda (keymap)
       (char-set-for-each
        (lambda (c) (define-key keymap (kbd c) insert-char))
        (char-set-union char-set:graphic
                        char-set:blank)))
     keymaps)))

(initialize-keymaps)
(define-key global-keymap (kbd "C-a") move-beginning-of-line)
(define-key global-keymap (kbd "C-e") move-end-of-line)
(define-key global-keymap (kbd "C-f") forward-char)
(define-key global-keymap (kbd "C-b") backward-char)
;; (define-key global-keymap (kbd "<right>") forward-char)
;; (define-key global-keymap (kbd "<left>") backward-char)
(define-key global-keymap (kbd "C-k") kill-line)
(define-key global-keymap (kbd "<backspace>") delete-backward-char)
(define-key global-keymap (kbd "C-d") delete-char)
(define-key global-keymap (kbd "RET") show-newline)

(define-key global-keymap (kbd "M-d") kill-word)
(define-key global-keymap (kbd "M-<backspace>") backward-kill-word)
(define-key global-keymap (kbd "M-f") forward-word)
(define-key global-keymap (kbd "M-b") backward-word)
