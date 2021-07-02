(let test
    (lambda ()
      (print message)))

(let echo
    (lambda (name)
      (let ((message "echo"))
        (print message name)
        (test))))

(echo "Hello, world")
