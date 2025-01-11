(import hy.reader [read-many])
(import hy [eval])

;; Define the function that inserts a log expression into the function definition
;; read-many returns generator, next assumes the first sexp will be the single fn we want
(defn add-logging [func-string log-expression]
  ;; Parse the function string into a Hy expression and destructure using `let`
  (let [[_ name params #* body] (next (read-many func-string))]
    ;; Reconstruct the function definition with the log statement inserted
    `(defn ~name ~params
       ~log-expression
       ~@body)))

;; Original function as a string to convert
(setv foo-string "(defn foo [x] (print x) (+ x x))")  ; Function definition as a string

;; Define the log expression using natural Hy code
(setv inject-code '(print "Executing foo"))

;; Add logging to the function
(setv transformed-exp
  (add-logging foo-string
               inject-code))  ; Ensure quotes remain intact

;; Show the transformed code for debugging
(print transformed-exp)

;; Evaluate the transformed function definition
(eval transformed-exp)

;; Test the transformed function.
;; Expected Output:
;; Executing foo
;; 10
;; 20
(print (foo 10))
