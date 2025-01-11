;; Define a macro to add a logging statement to a function
(define-syntax add-logging
  (syntax-rules ()
    ((_ (name params body ...) log-message)
     (define (name . params)
       (begin
         (display log-message)  ; Print the log message
         (newline)
         body ...)))))

;; Use the macro to define a function
(add-logging (foo (x) (+ x x)) "Executing foo")

;; Test the function
(display (foo 10)) ; Expected output:
                   ; Executing foo
                   ; 20
(newline)
