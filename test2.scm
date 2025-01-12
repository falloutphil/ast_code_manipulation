(use-modules (ice-9 rdelim))  ; Import the module for read-line if needed for user input

(define-syntax add-logging
  (syntax-rules ()
    ((_ (define (name . params) . body) log-expression)
     (define (name . params)
       log-expression
       . body))))

;; Usage Example

;; Original function as a quoted form
(add-logging
 (define (foo x)
    (format #t "~a~%" x)
    (+ x x))
 (format #t "Executing foo~%"))

;; Test the transformed function
(format #t "~a~%" (foo 10))
