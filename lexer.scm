(let echo      ; Global scope
    (lambda (name)
      (let ((string "YEAH")) ; Local scope
        (print name string))))

(echo "lol")   ; Scope
