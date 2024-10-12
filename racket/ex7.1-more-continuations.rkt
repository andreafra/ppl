#lang racket

; In the woods, there are two roads, 'left and 'right
; Only one road leads you home
(define (adventure choice)
  (case choice
    [(left) (displayln "You get lost in the woods!") 'lost]
    [(right) (displayln "You find your way home!") 'home]))

(define checkpoint #f)
(define snacks 4)

(when (equal?
     'lost
     (adventure (call/cc
                 (λ (k)
                   (set! checkpoint k)
                   (set! snacks (- snacks 1))
                   (printf "You have ~a snacks left!\n" snacks)
                   'left))))
    (checkpoint 'right))
(printf "You have ~a snacks left!\n" snacks)
