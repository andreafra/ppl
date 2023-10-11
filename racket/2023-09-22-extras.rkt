#lang racket

;; ======= EXTRAS =======
; stuff I might have forgot, explained poorly or too fast!

;; Defining a variable:
;; as is common in functional programming, we cannot
;; reassign values to an already defined top-level variable.
(define a 'apple)
; (define a 2) ; Error: already defined
;; but you can change its value with 'set!'.
;; The '!' means a procedure has side-effects
(set! a 'banana)
a

;; 1. scoping
;; Just over on 'x' in DrRacket and see
(define x 5)

(define (change-value x) ; what happens if we rename just this 'x'?
  (set! x 2)
  (displayln x))
(change-value x)
x ; what's the value of 'x', 2 or 5?

(define v (vector 1 2 3)) ; we can't use #(1 2 3) bc it's immutable

(define (change-vector v)
  (vector-set! v 1 0) ; change item '2' to '0'
  (displayln v))
(change-vector v)
v ; what do we expect the second value to be, 2 or 0? why?

;; Static Scoping: guess the value of k returned by 'f' called by 'g'
(define k 10)
(define (f)
  k) ; 'f' is called in 'g', but the value of 'k' is the top-level one.
(define (g)
  (define k 5) ; this is useless
  (f))
(g) ; call 'g' that calls 'f' ; => 10
; if you answered 5, you are thinking of dynamic scoping.
; Lucky for you, Racket/Scheme do not have dynamic scoping like
; other languages in the LISP family do.

;; 2. let
(define ada "Ada")
ada
(let ([bob "Bob"])
  (displayln ada)
  (displayln bob))
;; but we cannot call 'bob' since it's not in scope
; bob ; => not defined

;; 3. 'named let'
; a fancy way to do loops
(define (loop-ten-times)
  (let loop ([i 0])
    (when (<= i 10)
      (displayln (string-append "Loop " (~a i) " times"))
      (loop (+ i 1)))))
(loop-ten-times)
;; Loop here behaves similarly to a procedure call
(define (loop-ten-times-alt)
  (let ([i 0]) ; we can take the 'i' outside...
    (let loop () ; ... but we keep the empty list!
      (when (<= i 10)
        (displayln (string-append "Loop-alt " (~a i) " times"))
        (set! i (+ i 1))
        (loop)))))
(loop-ten-times-alt)

; we could do this with defines, too
(define (loop-ten-times-def)
  (define (loop i)
    (when (<= i 10)
      (displayln (string-append "Loop-def " (~a i) " times"))
      (loop (+ i 1))))
  (loop 0)) ; we need to call it, though
(loop-ten-times-def)

;; All of the examples above happens to be tail-recursive!
;; Of course, you cannot call 'loop' outside of the 'loop-ten-times-*'
;; procedure.

;; 4. '.' in procedure argument list
; ALSO when to use 'apply'
(define (split-sum x . xs)
  (displayln x) ; => 1
  (displayln xs)  ; => '(2 3 4 5)
  (+ x
     (apply + xs))) ; = (+ <the contents of xs>)
                    ; = (+ 2 3 4 5)
(split-sum 1 2 3 4 5)

;; 5. foldl and foldr
; When I talked about foldl and foldr complexity, I forgot to mention
; that it was SPATIAL COMPLEXITY! Time complexity is of course O(n).

; lists are just like linked list, and you keep the reference to the head of the list!
; myList = x -> y -> z -> null
; myList = '(x . (y . (z . ()))