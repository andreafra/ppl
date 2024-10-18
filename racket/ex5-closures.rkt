#lang racket

;; CLOSURE
; requires a
; - function:    lambda/procedure
; - environmemt: local variables
; - scope: the closure has access to all the variables
;          that were in scope at the time of creation.

(define (make-counter)
  (let ((count 0)) ; environment - can't be accessed from outside the instance of 'make-counter'
    (lambda () ; return a function - state is kept when changing the variables in the environment
      (set! count (+ 1 count))
      count)))

(define counter1 (make-counter))
(define counter2 (make-counter))

(displayln (counter1)) ; => 1
(displayln (counter1)) ; => 2
(displayln (counter2)) ; => 1
(displayln (counter1)) ; => 3
(displayln (counter2)) ; => 2

;; If you are familiar with JavaScript and ReactJS hooks, the 'useState' hook is a closure.
;; https://medium.com/swlh/learn-by-implementing-reacts-usestate-and-useeffect-a-simplified-overview-ea8126705a88
