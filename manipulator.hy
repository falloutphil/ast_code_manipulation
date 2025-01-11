;; Import necessary functions
(import hy.reader [read-many])
(import hy [eval])
(import hy.models [Expression Symbol String])

;; 1) Define the function that inserts a log expression into the function definition
(defn add-logging [func-string log-expression]
  ;; Parse the function string into a Hy expression
  (setv raw-forms (list (read-many func-string)))  ; Convert the lazy result to a list
  (setv func (get raw-forms 0))                    ; Get the first top-level form
  ;; Extract function components
  (setv name (get func 1))        ; 'foo'
  (setv params (get func 2))      ; ['x']
  (setv body (cut func 3 None))   ; Use `cut` to slice from index 3 onward

  ;; Reconstruct the function definition with the log statement
  `(defn ~name ~params
     ~log-expression
     ~@body))

;; 2) Define the function as a string
(setv foo-string "(defn foo [x] (print x) (+ x x))")  ; Function definition as a string

;; we can't use a basic sexp here!
(setv inject-code
      (Expression [(Symbol "print") (String "Executing foo")]))

;; 3) Add logging to the function
(setv transformed-fn
  (add-logging foo-string inject-code))  ; Ensure quotes remain intact

;; 4) Show the transformed function for debugging
(print transformed-fn)
(print "\n")

;; 5) Evaluate the transformed function definition
(eval transformed-fn)

;; 6) Test the transformed function
(print (foo 10))  ; Should print "Executing foo", then "10", then "20"
