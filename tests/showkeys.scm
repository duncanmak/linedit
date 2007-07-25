(with-current-input-terminal-mode 'raw
  (let loop ()
    (let* ((c  (read-char)))
      (format #t "You typed *~A (~A)*~%" c (char->ascii c))
      (if (char=? c #\q) 
          (display "bye bye now\n")
          (loop)))))