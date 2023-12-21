#lang racket

;;; 2019.07.24
; Write a functional, tail recursive implementation of a procedure that takes a list of numbers L and two values x and y, and returns three lists: one containing all the elements that are less than both x and y, the second one containing all the elements in the range [x,y], the third one with all the elements bigger than both x and y. It is not possible to use the named let construct in the implementation.

(define (split-in-ranges-helper ls x y bf bt aft)
  (if (empty? ls)
      (list bf bt aft)
      (let ((el (car ls)))
        (cond
          [(and (< el x) (< el y)) (split-in-ranges-helper (cdr ls) x y (cons el bf) bt aft) ]
          [(and (> el x) (> el y)) (split-in-ranges-helper (cdr ls) x y bf bt (cons el aft)) ]
          [else (split-in-ranges-helper (cdr ls) x y bf (cons el bt) aft) ]) )))

(define (split-in-ranges ls x y)
  (split-in-ranges-helper ls x y '() '() '()))