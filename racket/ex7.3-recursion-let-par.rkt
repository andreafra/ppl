#lang racket

(define x 0)

(let ((y 0) (cc (call/cc (λ (k) (k k)))))
  (set! x (+ 1 x))
  (set! y (+ 1 y))
  (printf "X=~a\n" x)
  (printf "Y=~a\n" y)
  (newline)
  (if (< x 3)
      (cc cc)
      x))
