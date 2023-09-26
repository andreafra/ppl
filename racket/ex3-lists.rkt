#lang racket

(define numbers '(1 2 3 4 5 6 7 8))

;; Higher-order Functions
; A higher-order function (HOF) is a function that does at least one of the following:
; - takes one or more functions as arguments
; - returns a function as its result.

;; MAP
; (map proc lst)
(map (λ (x) (+ x 1)) numbers)

;; FOLDL - space complexity: O(1)
; (foldl proc init lst)
; proc = (λ (x y z ... acc) body)
; init = initial value of the accumulator ('acc')
; lst  = xs ys zs ...
; we can fold multiple lists at the same time
; NOTE: It's tail-recursive

; sums all numbers
(foldl + 0 numbers)

(define show-sum (λ (x acc)
         (displayln (string-append "x = " (~a x) "; acc = " (~a acc)))
         (+ x acc)))

(foldl show-sum
       0
       numbers)

;; FOLDR - space complexity O(N)
; works just like 'foldl' but starts at the END of the list
; also, NOT tail-recursive
(foldr show-sum
       0
       numbers)

;; REVERSE A LIST
; btw, don't call any argument 'list' otherwise you're
; overwriting the 'list' procedure. Use 'lst' instead.
(define (list-reverse lst)
  (define (list-reverse-helper x acc)
    (displayln (string-append "x = " (~a x) "; acc = " (~a acc)))
    (if (null? x)
        acc
        (list-reverse-helper (cdr x) (cons (car x) acc))))

  (list-reverse-helper lst '()))

(list-reverse numbers)

; reverse a list with foldl

(define (list-reverse-with-foldl lst)
  (foldl (λ (x acc)
           (if (null? x)
               acc
               (cons x acc)))
         '() lst))

(list-reverse-with-foldl numbers)

;; FACTORIAL
(foldl * 1 numbers)

;; FLATTEN A LIST
(define (list-flatten lst)
  (cond [(null? lst) lst]
        [(not (list? lst)) (list lst)] ; '(lst) works as well
        [else (append (list-flatten (car lst)) (list-flatten (cdr lst)))]))

(list-flatten '(1 (2) (3 4) (((5)) 6 7) 8))

;; MERGE-SORT
; first we need a 'merge' procedure that merges two lists
; 
(define (merge lst1 lst2)
  (cond [(null? lst1) lst2]
        [(null? lst2) lst1]
        ; take first element from both lists, put the smallest first,
        ; then recursively repeat
        [else (let ((head1 (car lst1))
                    (rest1 (cdr lst1))
                    (head2 (car lst2))
                    (rest2 (cdr lst2)))
                (if (<= head1 head2)
                    (cons head1 (merge rest1 lst2))
                    (cons head2 (merge lst1 rest2))))]))

(merge '(1 3 5) '(2 4 4 6))  

; then we need a 'halve' procedure to split the list in half, returning a (cons left right)
; where 'left' and 'right' are the two halves of the list.
; you don't really have to reinvent the wheel: (split-at lst pos)
; Alternatively, we can make our own version:
; (take lst n) returns the first n elements of lst;
; (drop lst n) returns all the elements of lst after the n position.
(define (halve lst)
  (let* ((mid (quotient (length lst) 2)))
    (let ((left (take lst mid))
          (right (drop lst mid)))
      (cons left right))))

; alternatively:
; 'split-at' returns 2 lists, so we need a way to assign each to a variable
; let-values allows us to do that
(define (halve2 lst)
  (let-values (((left right) (split-at lst (quotient (length lst) 2))))
    (cons left right)))
    
(halve numbers)
(halve2 numbers)

(define (merge-sort lst)
  (cond [(null? lst) lst] ; empty list
        [(null? (cdr lst)) lst] ; list of 1 element
        [else (let ((halves (halve2 lst)))
                (merge (merge-sort (car halves))
                       (merge-sort (cdr halves))))]))

(merge-sort '(4 2 5 1 8 6 7 3 9))

; using 'let-values' and 'split-at' we can get a more elegant solution,
; but this is enough.

;; FIZZBUZZ
; For each number from 1 to 99,
; print 'Fizz' if the number is divisible by 3
; print 'Buzz' if the number is divisible by 5
; print 'FizzBuzz' if the number is divisible by both 3 and 5
; else, just print the number
; e.g.: 1 2 Fizz 4 Buzz Fizz 7 8 Fizz 10 ...
(define (divisible-by? n m)
  (zero? (modulo n m)))

(define (fizzbuzz n)
  (cond [(divisible-by? n 15) "FizzBuzz"]
        [(divisible-by? n 5)  "Buzz"]
        [(divisible-by? n 3)  "Fizz"]
        [else n]))
(map fizzbuzz (range 1 100))