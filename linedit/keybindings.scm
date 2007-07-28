;;; -*- Mode: Scheme; scheme48-package: linedit -*-
(define (initialize-keymaps . args)
  (let-optionals args ((keymaps (list global-keymap)))
    (for-each
     (lambda (keymap)
       (char-set-for-each
        (lambda (k) (define-key keymap k insert-char))
        (char-set-union char-set:graphic
                        char-set:blank)))
     keymaps)))

(define-syntax define-key
  (syntax-rules ()
    ((_ keymap key action)
     (table-set! keymap (char->ascii key) action))))

(define-key global-keymap (kbd "ESC") lookup-meta-keymap)
(define-key global-keymap (kbd "C-a") move-beginning-of-line)
(define-key global-keymap (kbd "C-e") move-end-of-line)
(define-key global-keymap (kbd "C-f") forward-char)
(define-key global-keymap (kbd "C-b") backward-char)
(define-key global-keymap (kbd "C-k") kill-line)
(define-key global-keymap (kbd "<backspace>") delete-backward-char)
(define-key global-keymap (kbd "C-d") delete-char)
(define-key global-keymap (kbd "RET") show-newline)

;; (define-key meta-keymap (kbd "M-d") kill-word)
;; (define-key meta-keymap (kbd "M-f") forward-word)
;; (define-key meta-keymap (kbd "M-b") backward-word)
