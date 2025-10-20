#lang racket
;; JAVASCRIPT STYLE OO
; https://developer.mozilla.org/en-US/docs/Learn/JavaScript/Objects/Object_prototypes
; we need some macros to make this easier
;; 1. CREATE
(define-syntax obj-create
  (syntax-rules ()
    [(_)
     (make-hash '(["__proto__" . #f]))]
    [(_ PROTOTYPE)
     (make-hash (list [cons "__proto__" PROTOTYPE]))])); accept a list of pairs as default values
;; 2. SETTER
(define (obj-set object key new-val)
  (hash-set! object key new-val))
;; 3. GETTER
(define (obj-get object key)
  (hash-ref object key))
; and a special one for prototypes
(define (obj-get-prototype object)
  (hash-ref object "__proto__"))
;; 4. CALL / SEND
(define (obj-find object key)
  (hash-ref object key
            (λ ()
              (let ([prototype (obj-get-prototype object)])
                (if prototype
                  (obj-find prototype key)
                  (error "no valid implementation found"))))))
    

(define-syntax-rule (obj-call object key args ...)
  (let ([method (obj-find object key)])
    (when (not (void? method))
        (method object args ...))))
;; Create some objects

; Object is the 'root', it will be the prototype of everything else
(define Object (obj-create))
(obj-set Object "toString" (λ (self) (displayln "[object Object]")))
Object
(obj-call Object "toString") ; this works
(obj-call Object "toStringgg") ; this does not
(newline)

; Create String prototype, extending
(define String (obj-create Object))
String
(obj-call String "toString")
(newline)
(obj-set String "toString" (λ (self)
                           (displayln (obj-get self "value"))))

; Create an instance of String
(define my-string (obj-create String))
(obj-set my-string "value" "Hello!")
(obj-call my-string "toString")

