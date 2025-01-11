;; Define the function that inserts a log expression into the function definition
(define (add-logging func-string log-expression)
  ;; Parse the string into a list (e.g. '(define (foo x) (+ x x)))
  (let* ((func        (read (open-input-string func-string)))  ; e.g. '(define (foo x) (+ x x))
         (name-params (cadr func))    ; e.g. '(foo x)
         (body        (cddr func)))  ; e.g. '((+ x x))
                                      ;
    ;; Reconstruct with log-expression sexp inserted at the start of the body.
    `(define ,name-params
         ,log-expression
         ,@body)))

;; Our original function as a string, to convert
(define foo-string "(define (foo x) (format #t \"~a~%\" x) (+ x x))")

;; Define the log expression using quote+sexp
(define inject-code '(format #t "Executing foo~%"))

;; Add logging to the function and store sexp
(define transformed-sexp
  (add-logging foo-string
               inject-code))

;; Show the code for debugging
(format #t "~a~%" transformed-sexp)

;; Evaluate the code in a top-level environment so Guile treats
;; it as if typed at the REPL. This preserves the string literal properly.
(eval transformed-sexp (interaction-environment))

;; Test the transformed function.
;; Executing foo
;; 10
;; 20
(format #t "~a~%" (foo 10))
