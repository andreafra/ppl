#lang racket

;; (1 2 ((2) 3) 4) => (1 2 3 4)

(define (list-flatten lst)
  (cond [(null? lst) lst]
        [(not (list? lst)) (list lst)]
        [else (append (list-flatten (car lst)) (list-flatten (cdr lst)))]))

(list-flatten '(1 2 (2 (3)) (((4)))))


;; (multifun (f g) (x) ((+ x x x) (* x x)))
(define-syntax multifun
  (syntax-rules ()
    [(_ (f) (args ...) (body))
     (define (f args ...) body)]
    [(_ (f . fs) (args ...) (body . bodies))
     (begin
       (multifun (f) (args ...) (body))
       (multifun fs (args ...) bodies))]
    ))

(multifun (f g h)
          (x)
          ((+ x x x)
           (* x x)
           (- x x x)))

(f 2)
(g 2)
(h 2)

;; alternative solution
(define-syntax multifun2
  (syntax-rules ()
    [(_ (f) (args ...) (body))
     (define (f args ...) body)]
    [(_ (f fs ...) (args ...) (body bodies ...))
     (begin
       (multifun2 (f) (args ...) (body))
       (multifun2 (fs ...) (args ...) (bodies ...)))]
    ))

(multifun2 (f1 g1 h1)
          (x)
          ((+ x x x)
           (* x x)
           (- x x x)))

(f1 2)
(g1 2)