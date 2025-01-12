(use-modules (ice-9 rdelim))

(define-syntax do-while
  (syntax-rules ()
    ((_ test body ...)
     (begin
       body ...
       (while test
         body ...)))))

(define x 0)
(set! x (string->number (read-line)))

(do-while (> x 0)
  (begin
    (display "Looping...")
    (newline)
    (set! x (- x 1))))
