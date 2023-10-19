#lang racket

;; CALL/CC
; kinda like time-travel
; we're simply going back (or forward*) to a saved call stack.

(define (now)
  (call/cc
   (λ (k) ; 'k' is the continuation
     (k "Hey!"))))

(now) ; => Hey!
;; Is calling 'k' just like using 'return'? Absolutely not! In fact, we're never
;; returning from the call (k "Hey!").

;; A simple example:
(+ 1 (call/cc (λ (k) (k 2))))
;    ^ call/cc saves this "return point"! It's a place we can go back to later.
;      Where does it save it? Into 'k'!
;      When we call (k ...) we're going back at
;      that point in time and put the arg of 'k'
;      in its place.
;      so... (+ 1 2)

(define (list-fruits)
  (call/cc
   (λ (k)
     (displayln "apple")
     (k "banana")
     "carrot"))) ; we don't reach this point

(list-fruits) ; => apple, banana

;; Does calling a continuation affect other values?
(displayln "foo:")
(define foo-k #f) ; continuation
(define (foo)
  (let ([i 0])
    (displayln i)
    (call/cc (λ (k)
               (set! i 3)
               (displayln i) ; => 3
               (set! foo-k k)
               (set! i 5)
               (displayln i) ; => 5
               (foo-k)))
    (displayln i))) ; => 5

(foo)



;; Can you guess what these return?
;; 1
(call/cc (λ (k0)
           (+ 1 (call/cc (λ (k1)
                           (+ 1 (k0 3)))))))
;; 2
(call/cc (λ (k0)
           (+ 1 (call/cc (λ (k1)
                           (+ 1 (k0 (k1 3))))))))
;; 3
(call/cc (λ (k0)
           (+ 1
              (call/cc (λ (k1)
                           (+ 1 (k1 3))))
              (k0 1))))

;; SOLUTIONS
;; 1 => 3
;; 2 => 4
;; 3 => 1

;; It get worse...
(define x 0)

(let ([cc (call/cc (λ (k) (k k)))])
  (set! x (add1 x)) ; x += 1
  (displayln x)
  (if (< x 3)
      (cc cc) ; we're going back to the moment 'cc' is binded to the continuation 'k'.
              ; as long as we continue to call cc onto itself we have our portal back to
              ; top of the loop (just after the let)
              ; if we pass something else to 'cc', it must be a function because we're
              ; calling it. Try (cc 1) and it will fail.
      x))


;; RETURN - we can use call/cc to mimic the 'preemptive return' (just like in C)
; when we call it, we jump out of the function.

;; BREAK - we can use call/cc to break out of a loop
(define-syntax while-break
  (syntax-rules (break-id:)
    [(_ cond break-id: break body ...) ; 'break' will be replaced by the keyword we choose at runtime
     (call/cc (λ (break)
       (let loop ()
         (when cond
           (begin body ...)
           (loop)))))]))


(displayln "== BREAK ==")
(define y 5)
(while-break (> y 0) break-id: break ; <-- we need this last bit because of hygienic macros
                                     ; we can't call 'break' from outside the syntax-rule
             (when (= y 2) (break))
             (set! y (sub1 y)) ; y -= 1
             (displayln y))
; it will print 4 3 2 and then it will stop


;; -- HARD EXAMPLE --
;; GENERATORS - the 'yield' in Python/JavaScript
;; It's better to do it with simple closures, though!
(define (gen-one lst)
  ;; Both internal functions are closures over lst
  (define (state k-return) ; <- call k-return to return a value from the generator
    (for-each 
     (λ (elem)
       (set! k-return (call/cc (λ (k1) ; <- call k1 to continue in the for-each
                                 ;; Grab the current continuation
                                 (set! state k1)
                                 (k-return elem)))))
     lst)
    (k-return 'end))

  ;; This is the actual generator, producing one item from lst at a time.
  (define (generator)
    (call/cc state))

  ;; Return the generator 
  generator)

(define gen-num
  (gen-one '(0 1 2))) ; lst <- '(0 1 2)

(gen-num) ; 0 
(gen-num) ; 1
(gen-num) ; 2
(gen-num) ; 'end
