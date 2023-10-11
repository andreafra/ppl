#lang racket

;; Structs automatically create a set of procedures
; e.g. person? set-person-age?
;; SEE https://docs.racket-lang.org/reference/define-struct.html
(struct person
  (name
   [age #:mutable]))

(define p1 (person "Ada" 24))
(define p2 "Bob")

(person? p1)
(person? p2)

(set-person-age! p1 28)
(person-age p1)

;; Trees!
(struct node
  ([value #:mutable]))

(struct binary-node node
  (left
   right))

(define (leaf? n)
  (and (node? n) (not (binary-node? n))))

; let's make a simple tree
(define a-tree (binary-node 2 (binary-node 3 (node 4) (node 2)) (node 1)))

(define (print-tree n)
  (displayln (node-value n))
  (unless (leaf? n)
    (print-tree (binary-node-left n))
    (print-tree (binary-node-right n))))
(print-tree a-tree)

;; ====== DISPLAY TREE NICELY ======
; feel free to ignore this code
(require pict)
(require pict/tree-layout)
(define (tree-show n)
  (define (tree-show-helper n)
    (cond [(leaf? n) (tree-layout #:pict (text (~a (node-value n))))]
          [(binary-node? n) (tree-layout #:pict (text (~a (node-value n)))
                                         (tree-show-helper (binary-node-left n))
                                         (tree-show-helper (binary-node-right n)))]))
  (naive-layered (tree-show-helper n)))
;; =================================

; displays the tree graphically
(tree-show a-tree)

; make a apply higher-order function for trees!
; applies the function f(x) to the value 'x' of
; each tree node.

#`(define (tree-apply f n)
  (set-node-value! n (f (node-value n)))
  (unless (leaf? n)
           (begin
             (tree-apply f (binary-node-left n))
             (tree-apply f (binary-node-right n)))))

#`(print-tree a-tree)




(define (tree-apply f n)
  (set-node-value! n (f (node-value n)))
  (when (binary-node? n)
    (begin
      (tree-apply f (binary-node-left n))
      (tree-apply f (binary-node-right n))))
  n)

(tree-show
 (tree-apply add1 a-tree))

(print-tree a-tree)

; Optional: Make a (tree-map f n) procedure that
; does not operate 'in place' like (tree-apply f n) does.