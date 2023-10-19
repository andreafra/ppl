#lang racket
;; NON-DETERMINISM and CALL/CC
;; with CHOOSE & FAIL
;;
;; an example of call/cc without macros
;;
;; we're gonna implement two methods:
;; 'choose' will try choose a branch, and will let us
;; backtrack to another branch if we 'fail'

(define (is-sum-of sum)
  (unless (and (>= sum 0) (<= sum 10))
    (error "out of range" sum))
  (let ([x (choose '(0 1 2 3 4 5))]
        [y (choose '(0 1 2 3 4 5))])
    (displayln (string-append (~a x) "+" (~a y) "=" (~a sum) "?"))
    (if (= (+ x y) sum)
        (list x y)
        (begin
          (displayln "is-sum-of fail")
          (fail)))))

(define *paths* '())
;; utils
(define (push-path x)
  (set! *paths* (cons x *paths*)))
(define (pop-paths)
  (let ([p1 (car *paths*)])
    (set! *paths* (cdr *paths*))
    p1))

;; CHOOSE
;; give a list of choices, save a checkpoint before picking a branch
;; (i.e.: the first element of the list). If there are no more choices, fail.
(define (choose choices)
  (if (null? choices)
      (begin
        (displayln "choice fail")
        (fail)) ; we've run out of choices, we were on the wrong path
      (call/cc
       (λ (k)
         (push-path (λ () ; save checkpoint
                      ; backtrack to k, and choose again from the rest
                      (k (choose (cdr choices)))))
         (car choices))))) ; return the current choice

;; FAIL
;; rolls back to the point before branching
(define fail (λ ()
               (if (null? *paths*)
                   (error "no paths")
                   ;; double (()) because we're calling the returned continuation
                   ((pop-paths)))))

(is-sum-of 6)