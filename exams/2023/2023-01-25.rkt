#lang racket

(define *conts* '())

(define (push-cont k)
  (set! *conts* (append *conts* (list k)))
  ;(displayln *conts*)
  )

(define (for-each/cc cond lst body)
  (when (cons? lst)
    (let ([x (car lst)])
      ; from here...
      (call/cc (λ (k)
                 (when (cond x)
                   (push-cont k))
                 (body x)))
      ; ... to here we have the 'extra' part about continuations
      ; last line is just to implement the same old for-each.
      (for-each/cc cond (cdr lst) body)))) ; iterate on the next list items

(define (use-cc)
  (when (cons? *conts*) ; = (not (empty? *conts*))
    (let ([c (car *conts*)])
      (set! *conts* (cdr *conts*))
      (c))))

;; Test it:
(for-each/cc odd?
             '(1 2 3 4)
             (λ (x) (displayln x)))

(displayln "----")
(use-cc) ; => 2 3 4 (and pushes another 'from 4' continuation in the queue)
(displayln "----")
(use-cc) ; => 4
(displayln "----")
(use-cc) ; => 4
(displayln "----")
(use-cc) ; => ''


