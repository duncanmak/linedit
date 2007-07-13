(with-current-input-terminal-mode 'raw
  (let loop ()
    (let* ((k  (get-keychord))
           (c  (keychord:key k))
           (s  (keychord->string k)))
      (format #t "You typed ~A~%" s)
      (if (char=? c #\q)
          (display "bye bye now\n")
          (loop)))))