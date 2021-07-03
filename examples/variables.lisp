(let v "global")                        ; Define the variable
(print v)                               ; Global variable

(set v "GLOBAL")                        ; Change the value of the variable
(print v)                               ; Global variable

(let ((v "local"))                      ; Define the variable in local scope
  (print v)                             ; Local variable
  (set v "LOCAL")                       ; Change the value of the variable in the local scope
  (print v))                            ; Local variable

(print v)                               ; Global variable
