#lang racket

;; EXCEPTIONS ARE ALREADY IMPLEMENTED IN RACKET
;; but it's good to know how to do it ourselves

(define *handlers* '())
; *...* (called earmuffs) is a convention for mutable globals

(define (push-handler proc)
  (set! *handlers* (cons proc *handlers*)))

(define (pop-handler)
  (let ([head (car *handlers*)])
    (set! *handlers* (cdr *handlers*))
    head))

(define (throw x)
  (if (pair? *handlers*) ; = (not (empty? *handlers*))
      ((pop-handler) x) ; calling the return of 'pop-handler' on 'x'
      (apply error x)))

; example:
; (push-handler displayln)
; (throw 5) ; => 5
; (throw 5) ; => error

(define-syntax try
  (syntax-rules (catch)
    [(_ expr ...
        (catch exception-id exception-body ...))
     (call/cc (λ (exit)
                ; install the handler
                (push-handler (λ (x) ; 'x' is the arg of 'throw'
                                (if (equal? x exception-id)
                                    (exit
                                     (begin exception-body ...))
                                    ; else
                                    (throw x))))
                (let ([res (begin expr ...)])
                  (pop-handler)
                  res)))]))

(define (foo)
  (displayln "Foo")
  (throw "bad-foo"))

(try
 (displayln "Before foo")
 (foo)
 (display "After foo") ; unreached code
 (catch "bad-foo"
        ; handle the exception here
        (displayln "I caught a throw.")
        #f))
; if we try to catch an unregistered exception, we'll have an error: contract violation