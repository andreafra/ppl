#lang racket
;; Object-Oriented Programming

;; The return of CLOSURES
;; we can use closures to scope attributes and methods
;; to an instance.
;; We want to make an equivalent of the following class
#|
class Person {
    public name: string
    private age:  int

    // constructor
    Person(name, age) {
        this.name = name
        this.age = age
    }

    int growOlder(years: int) {
        this.age += int
    }
}

Person bob = Person(Bob, 25)
bob.getName()

(define bob ...)
(bob 'name) => "Bob"
|#
(define (new-person
         initial-name ;; initial values / constructor
         initial-age)
  ;; attributes
  (let ([name initial-name]
        [age initial-age])
  ;; methods
    (define (get-name) ; getter for public attribute
      name)
    (define (grow-older years) ; a method to change age (and return it)
      (set! age (+ age years))
      age)
    (define (show) ; another method
      (display "Name: ")(displayln name)
      (display "Age: ")(displayln age))
    ;; dispatcher (to handle calls to methods)
    (λ (message . args)
      (apply (case message
               [(get-name) get-name]
               [(grow-older) grow-older]
               [(show) show]
               [else (error "unknown method")])
             args))))

(define ada (new-person "Ada" 25))
(define bob (new-person "Bob" 25))
(ada 'grow-older 10) ; => 35
(bob 'get-name) ; => "Bob"
(ada 'show)
(bob 'show)

;; Inheritance
(define (new-superhero name age init-power)
  (let ([parent (new-person name age)] ; inherits attrs/methods
        [power init-power])
    (define (use-power)
      (display name)(display " uses ")(display power)(displayln "!"))
    (define (show)
      (parent 'show)
      (display "Power: ")(displayln power))

    (λ (message . args)
      (case message
        [(use-power) (apply use-power args)]
        [(show) (apply show args)] ; overrides Person.show
        [else (apply parent (cons message args))]))))
        
(define superman (new-superhero "Clark Kent" 32 "Flight"))
(superman 'use-power)
(superman 'grow-older 10)
(superman 'show)

;; PROTOTYPE-BASED OBJECTS
; under the hood, it's just an hashmap
; with attrs/method names as KEY and
; their value/implementation as VALUE
(define new-obj make-hash)
(define clone hash-copy)

;; We need to use MACROS because otherwise we cannot
;; quote 'msg' correctly: if we quote 'msg' at runtime
;; we are quoting 'msg' itself: 'msg
;; When we use a macro
;; BTW: define-syntax-rule allows you to define a macro
;; that binds just one single pattern.
;; SETTER
(define-syntax-rule (obj-set object msg new-val)
  (hash-set! object 'msg new-val))
;; GETTER
(define-syntax-rule (obj-get object msg)
  (hash-ref object 'msg))
;; SEND MESSAGE
(define-syntax-rule (obj-send object msg arg ...)
  ((hash-ref object 'msg) ; retrieve method
   object arg ...)) ; call method with the object itself as first argument, and any arg

(define carl (new-obj))
(obj-set carl name "Carl")
(obj-set carl show
         (λ (self)
           (display "Name: ")(displayln (obj-get self name))))
(obj-set carl say-hi
         (λ (self to)
           (display (obj-get self name))(display " says hi to ")(displayln to)))

(obj-send carl show)
(obj-send carl say-hi "Dan")

; let create a new object that copies the properties of carl
(define dan (clone carl))
(obj-set dan name "Dan") ; this could be a setter method, if you want
(obj-send dan show)
(obj-set dan dance (λ (self)
                     (displayln "I, Dan, can dance")))
(obj-send dan dance)
(obj-send carl dance) ; => error