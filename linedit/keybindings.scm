;;; -*- Mode: Scheme; scheme48-package: linedit -*-

(define-syntax define-key
  (syntax-rules ()
    ((_ keymap key action)
     (table-set! keymap (char->ascii key) action))))

(define-key global-key-map (key "ESC") (lambda (k l) (lookup meta-key-map k l)))
(define-key global-key-map (key "C-a") move-beginning-of-line)
(define-key global-key-map (key "C-e") move-end-of-line)
(define-key global-key-map (key "C-f") forward-char)
(define-key global-key-map (key "C-b") backward-char)
(define-key global-key-map (key "C-k") kill-line)
(define-key global-key-map (key "<backspace>") delete-backward-char)
(define-key global-key-map (key "<delete>") delete-char)
(define-key global-key-map (key "C-d") delete-char)
(define-key global-key-map (key "RET") (lambda (k l) (newline) l))

;; (define-key meta-key-map (key "M-d") kill-word)
;; (define-key meta-key-map (key "M-f") forward-word)
;; (define-key meta-key-map (key "M-b") backward-word)
