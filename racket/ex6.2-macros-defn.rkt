#lang racket

; Let's define a copy of Clojure (another LISP) 'defn'
; defn allows you to match the arguments
; (defn FN-NAME
;   ([] BODY-0)
;   ([PARAM-1] BODY-1)
;   ([PARAM-2.1 PARAM-2.2] BODY-2))

(define-syntax defn
  (syntax-rules ()
    [(_ NAME (PARAMS BODY ... ) ...) ; PARAMS is a list, BODY ... is a series of S-EXPRs.
     (begin
       ; debug
       (displayln 'NAME)
       (display "Params: ")
       (displayln '('PARAMS ...))
       (display "Body: ")
       (displayln '('BODY ... ...)) ; why? one it's iterating all (PARAMS BODY) ..., the other just BODY ...
       ; define ONCE the function and make it receive any number of args (. arg-v)
       (define (NAME . arg-v)
         ; debug
         (display "Calling ")(display 'NAME)(display arg-v)(displayln ":")
         ; get the size of the arguments
         (let ([arg-n (length arg-v)])
           ; we can't use CASE. Why? CASE compares quoted values of the datum, so it can't calculate (length 'PARAMS)
           (cond
             ; register a handler (lambda) for each arity, and apply the arguments to it
             [(= arg-n (length 'PARAMS)) (apply (lambda PARAMS BODY ...) arg-v)] ...))))]))

(defn f
  ([] (display "0 ARGS") (displayln " and something else"))
  ([a] (displayln "1 ARGS"))
  ([a b] (displayln "2 ARGS")))

(newline)(newline)

(f)
(f 42)
(f 11 12)
