#lang racket

;; 1. Let's start with the evergreen "Hello world!"
(displayln "Hello world!") ;; => "Hello world!"

;; 2. Now let's make a 'procedure' (or function) to say hello
(define (say-hello)
  (displayln "Hello again!"))
;; We then need to call it...
;; Q: What happens if we don't wrap 'say-hello' in parentheses?
say-hello ;; => #<procedure:say-hello>
;; A: It returns the reference to the say-hello function.
(say-hello) ;; => "Hello again!"

;; 3. Let's make a procedure to greet someone by name
(define (greet name)
  (displayln (string-append "Hello " name "!")))

(greet "Ada")

;; we can define a 'variable' at top level with 'define' too!
(define anotherName "Bob")
(greet anotherName)
;; the define for procedures (define (name arg1 arg2 ...) body)
;; is syntactic sugar for (define name (lambda (arg1 arg2 ...) body))
;; Lambdas are anonymous procedures, and are a VALUE
;; P.S.: You can also use 'λ' instead of 'lambda' [ CTRL/CMD + \ ]

;; 4. We now want to greet only 'Ada' but not 'Bob'
(define (greet-ada name)
  (if (equal? name "Ada")
      (displayln "Hello again Ada!")
      (displayln "Go away Bob!")))

(greet-ada "Ada")
(greet-ada "Bob")
;; What happens if we use 'eq?' instead of 'equal?' ?
;; (eq? "A" (make-string 1 #\A))

;; What happens if we pass a number as input to 'greet'?
;; (greet 5) ;; error 'contract violation' means it expected something else i.e. a string

(define (safe-greet name)
  (if (string? name)
      (displayln (string-append "Hello " name "!"))
      (displayln "Cannot greet a number!")))

(safe-greet "Bob")
(safe-greet 5)