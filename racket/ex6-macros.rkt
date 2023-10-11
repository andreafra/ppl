#lang racket

(define (say-hello people)
  (displayln (string-join people)))

; (say-hello "ada" "bob") ; this does not work - can only accept 1 param
; passing a list is nice, but is there another way?

(define (say-hello-2 . people)
  ; dot is like: car . cdr
  ; if there is no 'car', every arg is in list people
  ; try replacing '. people' with 'person . people' ; => ("bob" "carl")
  (displayln (string-join people)))

(say-hello-2 "ada" "bob" "carl")

; we can use MACROS to define a new construct to say hello!
(define-syntax hello
  (syntax-rules ()
    [(_ names ...) ; <- 'names ...' matches a sequence of items like '"ada"', '"ada" "bob"'
       (displayln (string-append "Hello " (string-join (list names ...))))]))

(hello "ada" "bob" "carl")

; but MACROS allows us to do much more than that
; very good at optimizing performance, because they are expanded at compile time
; very good for prototyping language syntax and concepts: loops, exceptions, etc.
; careful: introducing poorly thought out macros will make the code unmaintanable!

(define-syntax while
  (syntax-rules () ; <- in this list, we put the literals that should not binded (decorative)
    [(_ cond body ...)
     (let loop ()
       (when cond
         (begin
           body ...
           (loop))))]))

(displayln "while loop")
(define i 0)
(while (< i 5)
       (set! i (add1 i))
       (displayln i))

; let's make a (for x in <list> <body>):
; where 'x' is in the scope of the body expression
(define-syntax for
  (syntax-rules (in)
    [(_ item in lst body ...)
     (begin
       (unless (list? lst) (error "Not a list"))
       (let loop ([item (car lst)]
                  [rest (cdr lst)])
         (begin
           body ...
           (unless (null? rest)
             (loop (car rest) (cdr rest))))))]))

(displayln "for-in loop")
(for x in '(1 2 3 4 5) (displayln x))
; (for x in "pizza" (displayln x)) ; => error: not a list

;; RECURSIVE MACROS
; you can call the macro itself after you define it.
; useful if you have multiple syntax rules that match different conditions

;; Multiple syntax rules
(define-syntax say
  (syntax-rules (hello goodbye)
    [(_ hello) (displayln "hello")]
    [(_ goodbye) (displayln "goodbye")]
    [(_ ...) (display "whatever...")] ; catch all case
    ))
(say hello) ; => hello
(say goodbye); => goodbye
(say thanks); => bad syntax