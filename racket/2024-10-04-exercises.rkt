#lang racket

(struct person
  (name
   (age #:mutable)))

(define p1 (person "Ada" 24))
(define p2 "Bob")

(person? p2)

(set-person-age! p1 25)

(person-age p1)

;; ------ TREES -----

(struct node
  ([value #:mutable]))

; Binary tree
(struct branch-node node
  (left
   right))

(define (leaf? n)
  (and (node? n) (not (branch-node? n))))

(define my-tree
  (branch-node
   2
   (branch-node
    3
    (node 4)
    (node 2))
   (node 1)))

(define (print-tree n)
  (displayln (node-value n))
  (unless (leaf? n)
           (print-tree (branch-node-left n))
           (print-tree (branch-node-right n))))

(print-tree my-tree)

; tree-apply f n
(define (tree-apply f n)
  (set-node-value! n (f (node-value n)))
  (when (branch-node? n)
      (tree-apply f (branch-node-left n))
      (tree-apply f (branch-node-right n))
      ))

(tree-apply (λ (x) (+ 1 x)) my-tree)

(newline)
(print-tree my-tree)

(newline)
;;

(and #t #f (even? 42) "a")

(define (my-and x . xs)
  (display x)(display " | ")(displayln xs)
  (if (null? xs)
      x
      (if x 
        (apply my-and xs)
        #f)))


(my-and  #t #f (even? 42) "a")

(newline)
;(and (expr-a) (expr-b) (expr-c))

;(if (expr-a)
;    (if (expr-b)
;        (expr-c)
;        #f)
;    #f)

(define-syntax my-and-m
  (syntax-rules (&&)
    [(_) #t]
    [(_ EXPR) EXPR]
    [(_ EXPR && OTHER-EXPRS ...)
     (if EXPR
         (my-and-m OTHER-EXPRS ...)
         #f)]))

(my-and-m  #t && #f && (even? 42) && "a")

;; (report-f expr)
;; - print the expr ; (* 1 2 3 4)
;; - return the expr ; 24

(define (report-f expr)
  (displayln 'expr)
  expr)

(report-f (* 1 2 3 4))

(define-syntax report-m
  (syntax-rules ()
    [(_ EXPR)
     (begin
       (display "Report: ")(displayln 'EXPR)
       EXPR)]))

(report-m (* 1 2 3 4))

(define x 0)

;(while (< x 10) break: break
;       (if (= x 7)
;           (break)
;           (set! x (+ 1 x))))

;(for x in '(1 2 4)
;  (displayln x))



(define-syntax defn
  (syntax-rules ()
    [(_ NAME (PARAMS BODY ...) ...)
     (displayln 'NAME)
     (display "Params: ")(displayln '('PARAMS ...))
     (display "Body: ")(displayln '('BODY ...))
     (define (NAME . arg-v)
       (display "Calling ")(display 'NAME)(displayln arg-v)
       (let ([arg-n (length arg-v)])
         (cond
           [(= arg-n (length 'PARAMS))
            (apply (lambda PARAMS BODY ...) arg-v)] ...)))]))
     
(defn my-func
  ([] (displayln "0 Args!"))
  ([a] (displayln "1 Args!"))
  ([a b] (displayln "2 Args!")))










