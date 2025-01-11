(import hy.reader [read-many])
(import hy [eval])
(import hy.models [Expression Symbol String])

;; Define the function that inserts a log expression into the function definition
(defn add-logging [func-string log-expression]
  ;; Parse the function string into a Hy expression
  (setv raw-forms (list (read-many func-string)))  ; Convert the lazy result to a list
  (setv func (get raw-forms 0))                    ; Get the first top-level form
  ;; Extract function components
  (setv name (get func 1))        ; 'foo'
  (setv params (get func 2))      ; ['x']
  (setv body (cut func 3 None))   ; Use `cut` to slice from index 3 onward

  ;; Reconstruct the function definition with the log statement expression
  `(defn ~name ~params
     ~log-expression
     ~@body))


;; Our original function as a string, to convert
(setv foo-string "(defn foo [x] (print x) (+ x x))")  ; Function definition as a string

;; we can't use a basic sexp in Hy!
(setv inject-code
      (Expression [(Symbol "print") (String "Executing foo")]))

;; Add logging to the function
(setv transformed-exp
  (add-logging foo-string inject-code))  ; Ensure quotes remain intact

;; Show the code for debugging
(print transformed-exp)

;; Evaluate the transformed function definition
(eval transformed-exp)

;; Test the transformed function.
;; Executing foo
;; 10
;; 20
(print (foo 10))
