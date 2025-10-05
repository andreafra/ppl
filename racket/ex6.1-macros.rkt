#lang racket

;; MACROS

; For example: (and (expr a) ... ) is a macro!

(and (even? 4) (string? 4)) ; => #f
(and #t (even? 2) (number? 2)) ; => #t

; BUT we can implement and as a function!
(define (my-and x . xs)
  (display x) (display " ") (displayln xs)
  (if (null? xs)
      x
      (when x 
        (apply my-and xs))))

(if (equal? (my-and #t (even? 4) (number? 4)) #t)
    (displayln "PASS")
    (displayln "FAIL"))

; Ideally, as a macro, we when we have:
;
; (and (expr a) (expr b) (expr c))
;
; we would rewrite it like this:
;
; (if (expr a)
;    (if (expr b)
;        (expr c)
;        #f)
;    #f)

(newline)(newline)

; LET'S GET BACK TO THE 'AND' MACRO

(define-syntax and-m
  (syntax-rules ()
    [(_) #t]
    [(_ COND) COND]
    [(_ COND OTHER-CONDS ...)
     (if COND
         (and-m OTHER-CONDS ...)
         #f)]))

(and-m)
(and-m 42)
(and-m 42 #t #f)

(newline)(newline)

; IS THERE ANYTHING WE CAN'T IMPLEMENT AS A FUNCTION,
; BUT REQUIRES A MACRO?

; let's say that we want to make a 'report' function
; that receives an expression, prints it, and returns
; its output

; Can a function do that?

(define (report-f lst)
  (displayln lst)
  lst)

(report-f (* 1 2 3 4))

; We need to use a macro!

(define-syntax report-m
  (syntax-rules ()
    [(_ EXPR)
     (begin
       (display "Report: ")(displayln (quote EXPR))
       EXPR)]))

(report-m (* 1 2 3 4))

