; 2022-07-06
Consider the technique “closures as objects” as seen in class, where a closure assumes the role of a class.
In this technique, the called procedure (which works like a class in OOP) returns a closure which is
essentially the dispatcher of the object.
Define the define-dispatcher macro for generating the dispatcher in an automatic way, as illustrated by
the following example:
(define (make-man)
    (let ((p (make-entity))
        (name "man"))
    (define prefix+name
        (lambda (prefix)
            (string-append prefix name)))
    (define change-name
        (lambda (new-name)
            (set! name new-name)))
    (define-dispatcher methods: (prefix+name change-name) parent: p)))
where p is the parent of the current instance of class man, and make-entity is its constructor.
If there is no inheritance (or it is a base class), define-dispatcher can be used without the parent: p part.
Then, an instance of class man can be created and its methods can be called as follows:
> (define carlo (make-man))
> (carlo 'change-name "Carlo")
> (carlo 'prefix+name "Mr. ")
"Mr. Carlo"

(define-syntax define-dispatcher (
    (syntax-rules (methods: parent:)
        [(_ methods: (METHOD ...) parent: P)
         (lambda (msg . args) 
            (case msg
                [(METHOD) (apply msg args)]) ...
            (else (apply P (cons msg args))) 
        )]
        [(_ methods: (METHODS))
         (define-dispatcher methods: METHODS parent: unknown-method)]

(define (unknown-method lst)
    (error "Unknown method" (car lst)))

;; 2023-01-25
We want to implement a for-each/cc procedure which takes a condition, a list and a body and performs a for-each.
The main difference is that, when the condition holds for the current value, the continuation of the body is stored in
a global queue of continuations. We also need an auxiliary procedure, called use-cc, which extracts and call the
oldest stored continuation in the global queue, discarding it.
E.g. if we run:
(for-each/cc odd?
    '(1 2 3 4)
    (lambda (x) (displayln x)))

two continuations corresponding to the values 1 and 3
will be stored in the global queue.
If you call (use-cc) -> 2 3 4


(define *scc* '())

(define (use-cc)
    ; get the oldest value
    ; and then call the continuation
    (when (cons? *scc*)
        (let ([k (car *scc*)]
              [ks (cdr *scc*)])
              (set! *scc* ks)
              (k))))

(define (for-each/cc cnd lst body)
    (when (cons? lst) ; also (and (list? lst) (not (empty? lst)))
        (let ([x (car lst)]
              [xs (cdr lst)])
            (call/cc (lambda (k)
                (when (cnd x)
                    ; save the cont. in the global queue
                    (set! *scc* (append *scc* (list k)))
                (body x))))
            (for-each/cc cnd xs body)

2022-02-10