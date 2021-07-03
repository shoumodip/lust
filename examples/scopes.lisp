(let test
    (lambda ()
      (echo "Not gonna work!")))

(let ((echo
       (lambda (name)
         (let ((message "echo"))
           (print message name)
           (test)))))
  (echo "Hello, world"))

(print "There is an error before this")
