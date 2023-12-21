#lang racket
;; -- OPTIONAL/ADVANCED -- (by M. Pradella)
;; 4) Define a better list comprehension macro, by exploiting for the
;; translation the List Monad seen in Haskell.

;; for example:
;; (monadic/co (cons x y) from
;;     x '(1 2 3 4)
;;     y '(-1 2 3))
;; returns:
;; ((1 . -1) (1 . 2) (1 . 3) (2 . -1) (2 . 2) (2 . 3) (3 . -1)
;;   (3 . 2) (3 . 3) (4 . -1) (4 . 2) (4 . 3))
;;
;; (monadic/co (cons x y)
;;     when (> x y) from
;;         x '(1 2 3 4)
;;         y '(-1 2 3))
;; returns:
;; ((1 . -1) (2 . -1) (3 . -1) (3 . 2) (4 . -1) (4 . 2)
;;   (4 . 3))

;; Solution:
;; first, remember that in the List Monad >>= is concatMap, while return is the
;; list containing the value returned

;; lst is a list of lists
(define (concat-map lst f)
  (apply append (map f lst)))

(define-syntax monadic/co
  (syntax-rules (when from)
    ((_ expr from v1 l1)
     (concat-map l1 (lambda (v1)
                      (list expr))))

    ((_ expr from v1 l1 v2 l2 ...)
     (concat-map l1 (lambda (v1)
                      (monadic/co expr from v2 l2 ...))))

    ((_ expr when condition
        from v1 l1)
     (concat-map l1 (lambda (v1)
                      (if condition
                          (list expr)
                          '()))))

    ((_ expr when condition
        from v1 l1 v2 l2 ...)
     (concat-map l1 (lambda (v1)
                      (monadic/co expr when
                                  condition from
                                  v2 l2 ...))))))