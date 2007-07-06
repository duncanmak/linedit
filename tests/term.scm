;;; -*- Mode: Scheme; scheme48-package: linedit-tests -*-
(with-raw-terminal
  (let loop ((input (read-char)))
    (display input)
    (if (eq? #\e input)
        #t
        (loop (read-char)))))
