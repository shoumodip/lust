(let dolist
  (macro
   (decl :rest body)
   `(let ((list ,(nth 1 decl))
          (,(car decl) nil))
      (while (not (nil? list))
        (set ,(car decl) (car list))
        (set list (cdr list))
        ,@body))))

(dolist (i '(1 2 3 4))
  (print i))
