(let variable "global value")           ; Define a global variable
(print variable)

(let ((variable "local value"))
  (print variable))                     ; Local scope

(do                                     ; Do block
    (print variable))

(print (if true                         ; If-else
           "Ok, this is expected!"
           "What the..."))

(if 2                                   ; If-else and Do
    (do
        (print "Yeah, 2 is true")
        (print "Rust devs be like: Bu.. But 2 cannot be used in if condition! REEEE! REEEE!")))

(if 0                                   ; Make fun of Lua!
    (print "Lua devs be like: What? This is crap! I bet this language even has crappy 0-based indices!")
    (print "Normal devs be like: well duh"))
