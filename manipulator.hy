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

  ;; Combine the log expression and the original body manually
  (setv new-body [log-expression])       ; Start with the log expression in a list
  (for [item body] (new-body.append item))  ; Append each part of the original body

  ;; Reconstruct the function definition with the log statement
  `(defn ~name ~params
     ~@new-body))

;; 2) Define the function as a string
(setv foo-string "(defn foo [x] (print x) (+ x x))")  ; Function definition as a string

;; 3) Add logging to the function
(setv transformed-fn
  (add-logging foo-string (Expression [(Symbol "print") (String "Executing foo")])))  ; Ensure quotes remain intact

;; 4) Show the transformed function for debugging
(print transformed-fn)
(print "\n")

;; 5) Evaluate the transformed function definition
(eval transformed-fn)

;; 6) Test the transformed function
(print (foo 10))  ; Should print "Executing foo", then "10", then "20"
