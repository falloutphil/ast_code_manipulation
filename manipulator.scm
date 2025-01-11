;; 1) Define the function that inserts a log expression into the function definition
(define (add-logging func-string log-expression)
  ;; Parse the string into a list (e.g. '(define (foo x) (+ x x)))
  (let* ((func        (read (open-input-string func-string)))  ; e.g. '(define (foo x) (+ x x))
         (name-params (cadr func))    ; e.g. '(foo x)
         (body        (cdddr func)))  ; e.g. '((+ x x))
    ;; Reconstruct with log-expression inserted at the start of the body.
    ;; We splice in log-expression plus the old body in a (begin ...).
    `(define ,name-params
         ,log-expression
         ,@body)))

;; 2) Our original function as a string, with correct syntax:
(define foo-string "(define (foo x) (newline) (display x) (newline) (+ x x))")

;; 3) Create the transformed definition: we pass `'(display "Executing foo")`
;;    so that the quotes stay intact in the final code.
(define transformed-sexp
  (add-logging foo-string
               '(display "Executing foo")))

;; 4) Show the code for debugging
(display transformed-sexp)
(newline)

;; 5) Evaluate the code in a top-level environment so Guile treats
;;    it as if typed at the REPL. This preserves the string literal properly.
(eval transformed-sexp (interaction-environment))

;; 6) Test the transformed function.
;;    We expect "Executing foo" and 10 (from display x) on one line, and 20 on the next because of last newline (from the x+x)
(display (foo 10))
(newline)
