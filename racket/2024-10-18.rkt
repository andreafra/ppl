#lang racket

(define (make-counter)
  (let ([count 0])
    (lambda ()
      (set! count (+ 1 count))
      count)))

(define counter1 (make-counter))
(define counter2 (make-counter))

(printf "C1: ~a\n" (counter1))
(printf "C1: ~a\n" (counter1))
(printf "C2: ~a\n" (counter2))
(printf "C1: ~a\n" (counter1))
(printf "C1: ~a\n" (counter1))
(printf "C2: ~a\n" (counter2))

(newline)(newline)

(define (new-person
         init-name
         init-age)
  ; state
  (let ([name init-name]
        [age init-age])
    ; method (public)
    (define (get-name)
      name)
    (define (grow-older years)
      (set! age (+ age years))
      age)
    (define (show)
      (printf "Name=~a; Age=~a" name age))
      
    ; dispatcher
    (lambda (msg . args)
      (apply (case msg
               [(get-name) get-name]
               [(grow-older) grow-older]
               [(show) show]
               [else (error "unknown method")])
             args))))

(define ada (new-person "Ada" 25))
(define bob (new-person "Bob" 24))

(ada 'grow-older 10)
(bob 'get-name)

(ada 'show)

; inheritance
(define (new-superhero name age init-power)
  (let ([parent (new-person name age)]
        [power init-power])
    (define (use-power)
      (printf "\n~a uses ~a!\n" name power))
    (define (show)
      (parent 'show)
      (display "Power: ") (displayln power))

    (λ (msg . args)
      (case msg
        [(use-power) (apply use-power args)]
        [(show) (apply show args)]
        [else (apply parent (cons msg args))]))))
(newline)
(define superman
  (new-superhero "Clark" 32 "Flight"))
(superman 'use-power)
(superman 'grow-older 35)
(superman 'show)
(newline)(newline)
; Prototype approach to OO

(define new-obj make-hash)
(define clone hash-copy)

(define-syntax-rule ; SET
  (obj-set object msg new-val)
  (hash-set! object 'msg new-val))
(define-syntax-rule ; GET
  (obj-get object msg)
  (hash-ref object 'msg))
(define-syntax-rule ; SEND
  (obj-send object msg args ...)
  ((hash-ref object 'msg) object args ...))

(define carl (new-obj))
(obj-set carl name "Carl")
(obj-set carl show
         (λ (self)
           (printf "Name=~a\n" (obj-get self name))))
(obj-set carl greet
         (λ (self to)
           (printf "~a says hi to ~a"
                   (obj-get self name) to)))

(obj-send carl show)
(obj-send carl greet "John")

(define dan (clone carl))
(obj-set dan name "Dan")
(newline)
(obj-send carl greet (obj-get dan name))

















