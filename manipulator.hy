(import hy.reader [read-many])
(import hy [eval])

;; Define the function that inserts a log expression into the function definition
(defn add-logging [func-string log-expression]
  ;; Parse the function string into a Hy expression and destructure using `let`
  (let [
        ;; Read the forms from the function string and extract the first form
        raw-forms (list (read-many func-string))
        func (get raw-forms 0)

        ;; Extract function components
        name (get func 1)        ; Function name, e.g., 'foo'
        params (get func 2)      ; Parameter list, e.g., '[x]'
        body (cut func 3 None)   ; Function body, e.g., '[(print x) (+ x x)]'
      ]
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
