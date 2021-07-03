(let test
    (lambda ()
      (echo "YEah")))

(let ((echo
       (lambda (name)
         (let ((message "echo"))
           (print message name)
           (test)))))
  (echo "Hello, world"))

(print "There is an error before this")
