; Import the `read-many` function from the `hy.reader` module.
; `read-many` parses a string of Hy code into a sequence of forms.
(import hy.reader [read-many])

; Define a helper function `coll?` to check if a given object is a collection.
; It returns True if `x` is an instance of `hy.models.Sequence` (Hy-specific sequence type)
; or a native Python `list`.
(defn coll? [x]
  (or (isinstance x hy.models.Sequence)
      (isinstance x list)))

; Define a helper function `contains?` to check if `item` is present in the collection `coll`.
; This replaces the missing `contains?` in Hy 1.x.
(defn contains? [coll item]
  (in item coll))

; Define a function `is-os-call?` that determines if an expression `expr` is a call to an OS function.
(defn is-os-call? [expr aliases direct-imports]
  (or
    ; Check for method calls like ('. os remove x)
    (and (coll? expr)
         (= (get expr 0) '.)
         (contains? aliases (get expr 1)))
    ; Check for direct function calls like (remove x)
    (and (coll? expr)
         (contains? direct-imports (get expr 0)))))

; Define a function `build-os-aliases` that processes import expressions to build sets of aliases
; and direct imports related to the `os` module.
(defn build-os-aliases [body]
  (setv aliases #{"os"})          ; Initialize aliases with 'os' as default
  (setv direct-imports #{})       ; Initialize an empty set for direct imports

  ; Iterate over each expression `expr` in `body`
  (for [expr body]
    ; Handle import statements like (import os) or (import [os fake_os])
    (when (and (coll? expr) (= (get expr 0) 'import))
      ; Iterate over each part after 'import'
      (for [import-part (cut expr 1)]
        (if (coll? import-part)
          ; If import-part is a collection, destructure it into module and alias
          (let [[module alias] import-part]
            (when (= module 'os)
              (aliases.add alias))) ; Add alias to `aliases` if module is 'os'
          ; If import-part is directly 'os', add 'os' to `aliases`
          (when (= import-part 'os)
            (aliases.add 'os)))))

    ; Handle from-import statements like (from os import remove) or (from os import [mkdir listdir])
    (when (and (coll? expr) (= (get expr 0) 'from))
      (let [module  (get expr 1)    ; Get the module name being imported from
            imports (cut expr 2)]     ; Get the list of imports after 'from' and module
        (when (= module 'os)
          ; Iterate over each import part
          (for [import-part imports]
            (if (coll? import-part)
              ; If import-part is a collection, destructure it to get the imported function
              (let [[imported] import-part]
                (direct-imports.add imported)) ; Add imported function to `direct-imports`
              ; If import-part is directly a function, add it to `direct-imports`
              (direct-imports.add import-part)))))))

  ; Return a dictionary with aliases and direct imports
  {"aliases" aliases "direct-imports" direct-imports})

; Define a function `contains-os-call?` that recursively checks if any expression within `body`
; (or nested within it) is an OS call.
(defn contains-os-call? [body aliases direct-imports]
  (any
    (for [expr body]
      (or (is-os-call? expr aliases direct-imports)
          (and (coll? expr)
               (contains-os-call? expr aliases direct-imports))))))

; Define a function `transform-function` that transforms a function definition based on whether
; it contains an OS call. If it does, the function body is replaced with an exception raise.
; Otherwise, a logging statement is added before the original body.
(defn transform-function [fn-spec aliases direct-imports]
  ; Extract function name, parameters, and body from `fn-spec`.
  ; If the body is missing (`None`), use an empty list to avoid errors.
  (setv name       (get fn-spec 1)    ; Get the function name (second element)
        params     (get fn-spec 2)    ; Get the function parameters (third element)
        maybe-body (cut fn-spec 3)    ; Get the rest as `maybe-body`
        body       (if maybe-body maybe-body [])) ; If `maybe-body` is `None`, set `body` to an empty list

  ; Check if the function body contains an OS call
  (if (contains-os-call? body aliases direct-imports)
    ; If it does, create a new function definition that raises an exception
    `(defn ~name ~params
       (raise (Exception (str "Function " ~name " contains a disallowed call to os"))))
    ; Otherwise, create a new function definition that adds a logging statement before the original body
    `(defn ~name ~params
       (print "Logging: executing " (str '~name))
       ~@body)))

; Define a function `transform-functions` that takes a string of Hy code, parses it,
; builds alias information, and transforms each function definition accordingly.
(defn transform-functions [fn-string]
  ; Initialize an empty list `forms` to store parsed forms
  (setv forms [])
  ; Iterate over each form `f` produced by `read-many` and append it to `forms`
  (for [f (read-many fn-string)]
    (forms.append f))

  ; Build alias and direct-import information from `forms`
  (setv aliases-info (build-os-aliases forms))

  ; Iterate over each function expression `fn-s-exp` in `forms`
  (for [fn-s-exp forms
        :if (and (coll? fn-s-exp) (= (get fn-s-exp 0) 'defn))] ; Filter to only function definitions (`defn`)
    (transform-function fn-s-exp (get aliases-info "aliases")
                                 (get aliases-info "direct-imports"))))

; Define the input string `function-string` containing import statements and function definitions.
(setv function-string "
(import os)
(import [os fake_os])
(from os import remove)
(from os import [mkdir listdir])

(defn safe [x] (+ x x))                                  ; A safe function that doubles `x`.
(defn unsafe1 [x] (os.remove x))                        ; An unsafe function that calls `os.remove`.
(defn unsafe2 [x] (fake_os.remove x))                   ; An unsafe function that calls `fake_os.remove`.
(defn unsafe3 [x] (remove x))                            ; An unsafe function that directly calls `remove`.
(defn nested-unsafe [x] (+ (fake_os.remove x) 1))        ; A nested unsafe function calling `fake_os.remove` inside an expression.
")

; Parse, transform, and define all functions from `function-string`.
(for [transformed (transform-functions function-string)] ; Iterate over each transformed function form
  (eval transformed))                                   ; Evaluate the transformed function to define it in the current environment

; Test all transformed functions by invoking them with a sample input and handling exceptions.
(for [fn-name ['safe 'unsafe1 'unsafe2 'unsafe3 'nested-unsafe]] ; Iterate over each function name
  (try
    (print (fn-name "/some/path"))                     ; Attempt to call the function with "/some/path" as an argument
    (catch [e Exception]                               ; If an exception occurs (e.g., disallowed OS call),
      (print e))))                                      ; catch it and print the exception message
