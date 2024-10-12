#lang racket

(define *my-k* #f)

(define (hi)
  (call/cc ; <- checkpoint
   (λ (k)
     (set! *my-k* k)
     (displayln "A")
     (k "C")
     "B")))

(hi)

(*my-k* 5)

(newline)(newline)

;; 1
(call/cc
 (λ (k0)
   (+ 1 (call/cc (λ (k1)
                   (+ 1 (k0 3))))))) ; => 3
;; 2
(call/cc
 (λ (k0)
   (+ 1 (call/cc (λ (k1)
                   (+ 1 (k0 (k1 3)))))))) ; => 4
;; 3
(call/cc (λ (k0)
           (+ 1
              (call/cc (λ (k1)
                         (+ 1 (k1 3))))
              (k0 1)))) ; => 1

(newline)(newline)

(define x 0)

(let ([cc (call/cc (λ (k) (k k)))])
  (set! x (+ 1 x)) ; x += 1
  (displayln x)
  (if (< x 3)
      (cc cc)
      x))

(newline)

(define *cont* '())
(define (return val) ((car *cont*) val))

;; Let's implement the RETURN
(define-syntax defret
  (syntax-rules (return:)
    [(_ (NAME ARGS ...) BODY ... )
     (define (NAME ARGS ...)
       (let ([result (call/cc (lambda (k)
                  (set! *cont* (cons k *cont*))
                  (begin BODY ...)))])
         (set! *cont* (cdr *cont*))
         result)
       )]))

(defret (fn x y)
  (if (= x 5)
      (return 42)
      (+ x y)))

(defret (gn x y)
  (if (= x 5)
      (return 10000)
      (+ x y)))

(fn 3 (fn 5 (gn 5 0)))
(fn 5 99)


; (for x from 1 to 10 BODY ...)
; skip the part where we handle "from 10 to 1"
(define *for-cont* #f)

(define (continue) (*for-cont*))

(define-syntax for
  (syntax-rules (from to)
    [(_ X from MIN to MAX BODY ...)
     (let loop ([X MIN])
       (when (< X MAX)
         (call/cc
           (λ (k)
             (set! *for-cont* k)
               (begin
                 BODY ...
                 )))
         (loop (+ 1 X))))]))



(for x from 1 to 10
  (if (= x 5)
      (continue)
      (displayln x)))














