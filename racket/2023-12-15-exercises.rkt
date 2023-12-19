#lang racket

;; DON'T FORGET TO STAR MY REPOSITORY ON
;; GITHUB, I    N E E D    THAT ACHIEVEMENT :)

#| 2022-01-21 -- Define a new construct called block-then which creates two scopes for
variables, declared after the scopes, with two different binding. |#

(define-syntax block
  (syntax-rules (then where <-)
    ((_ (body1 ...) then (body2 ...) where (var <- val1 val2) ...)
     (begin
       (let ((var val1) ...) body1 ...)
       (let ((var val2) ...) body2 ...)))))

(block
 ((displayln (+ x y))
  (displayln (* x y))
  (displayln (* z z)))
 then
 ((displayln (+ x y))
  (displayln (* z x)))
 where (x <- 12 3)(y <- 8 7)(z <- 3 2))
; Result should be: 20 96 9 10 6

#| 2023-01-25
We want to implement a for-each/cc procedure which takes a condition, a list and a body and performs a for-each.
The main difference is that, when the condition holds for the current value, the continuation of the body is stored in
a global **queue** of continuations.
We also need an auxiliary procedure, called use-cc, which extracts and call the
oldest stored continuation in the global queue, discarding it.
E.g. if we run:
(for-each/cc odd?
    '(1 2 3 4)
    (lambda (x) (displayln x)))
two continuations corresponding to the values 1 and 3
will be stored in the global queue.|#

(define cont-queue '())

(define (use-cc)
  (unless (empty? cont-queue)
    (let ((k (car cont-queue)))
      (set! cont-queue (cdr cont-queue))
      (k))))

(define (for-each/cc condition lst body)
  (for-each (lambda (vl)
              (body vl)
              (when (condition vl)
                (call/cc (lambda (k)
                           (set! cont-queue (append cont-queue (list k)))))))
            lst))

(define (for-each/cc2 cond lst body)
  (when (cons? lst)
    (let ([x (car lst)])
      (call/cc (lambda (k)
                 (when (cond x)
                   (set! cont-queue (append cont-queue (list k))))
                 (body x)))
      (for-each/cc2 cond (cdr lst) body))))