#lang racket

; basic recursion - not very performant
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (sub1 n))))) ; last op is a multiplication

; tail-recursive: the last operation performed is a
; procedure call.
(define (factorial-tail-rec n acc)
  (if (zero? n)
      acc
      (factorial-tail-rec (sub1 n) (* acc n)))) ; here

; visualize stack trace
(require racket/trace)
(trace factorial)
(trace factorial-tail-rec)

(factorial 5)
(factorial-tail-rec 5 1)

; we can also use named 'let' to perform a loop:
; (alternatively, we could use another 'define' to define factorial-loop)
(define (factorial-2 n)
  (let factorial-loop ((curr n)
                       (acc 1))
    (trace factorial-loop) ; just to show how 'factorial-loop' gets called
    (if (zero? curr)
      acc
      (factorial-loop (sub1 curr) (* acc curr)))))


(factorial-2 5)