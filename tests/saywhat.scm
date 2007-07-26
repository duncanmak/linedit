(setup-terminal)
(initialize-keymaps)
(display "Say what? ")
(format #t "You said: ~A~%" (process-line))
