#lang racket
(require rackunit)
;; lab5

;; Define a function called one that accepts a function and an argument and
;; applies the function to the argument.
;; the order of arguments: function first, then argument.
(define one 
  (lambda (f) 
    (lambda (x) 
      (f x))))


;; Define a function called two that accepts a function and an argument and
;; applies the function to the result of applying the function to the argument.
(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))


;; Define a function called zero that accepts a function and an argument and
;; returns the argument.
(define zero
  (lambda (f)
    (lambda (x)
      x)))


;; Define a function called add1 that accepts a number-like function and
;; returns a new number-like function that does the function "one more time".
(define add1
  (lambda (n)
    (lambda (f)
      (lambda (x)
        (f ((n f) x))))))


;; Define a function called ’add’ that accepts two functions like zero and one and
;; returns a function that applies its first argument to its second argument a number of times
;; that corresponds to the sum of the two ’numbers’ it was given.
(define add
  (lambda (n)
    (lambda (m)
      (lambda (f)
        (lambda (x)
          ((n f) ((m f) x)))))))

;; Define a function called tru that accepts two arguments and
;; returns the first one.
(define tru
  (lambda (x)
    (lambda (y)
      x)))


;; Define a function called fals that accepts two arguments and
;; returns the second one.
(define fals
  (lambda (x)
    (lambda (y)
      y)))


;; Define a function called ’if’ that accepts three arguments.
;; If the first one turns out to be the function tru (as above),
;; it returns the result of the second argument.
;; If the first one turns out to be the function fals (as above),
;; it returns the result of the third argument.
(define my-if
  (lambda (b) (lambda (then) (lambda (else) ((b then) else)))))


;; Write a program in OAZO5 that includes the definitions of ‘add‘ and ‘one‘ and ‘two‘ (as defined above), then
;; uses ‘add‘ and ‘two‘ and ‘one‘ to produce ‘three‘, then
;; applies it to a function that doubles a number, and
;; checks that the result is correct.
(module sim-OAZO5 racket
  (provide
   [rename-out (#%lam-app #%app)
               (my-if if)]
   else
   #%module-begin
   #%datum
   + - * / = equal? <= =>
   true false)
 
  (define-syntax (#%lam-app stx)
    (syntax-case stx (anon)
      [(_ anon (args ...) : body)
       #'(lambda (args ...) body)]
      [(_ e ...)
       #'(#%app e ...)]))
 
  (define-syntax (my-if stx)
    (syntax-case stx (then else)
      [(_ e1 then e2 else e3)
       #'(if e1 e2 e3)])))

(module my-module (submod ".." sim-OAZO5)
  
{if {equal? 8 {{{{{anon {num-like1} : {anon {num-like2} : {anon {f} : {anon {g} : {{num-like1 f} {{num-like2 f} g}}}}}}
              {anon {f} : {anon {x} : {f x}}}}
              {anon {f} : {anon {x} : {f {f x}}}}}
              {anon {x} : {* 2 x}}} 1}} then "works" else "doesn't work"})







(require 'my-module)













;; tests 
(define double (lambda (x) (* 2 x)))
(check-equal? ((zero double) 2) 2)
(check-equal? ((one double) 3) 6)
(check-equal? ((two double) 2) 8)
(check-equal? (((add1 one) double) 0) 0)
(check-equal? ((((add one) two) double) 2) 16)
(check-equal? ((tru 1) 2) 1)
(check-equal? ((fals 1) 2) 2)
(check-equal? (((my-if tru) 1) 2) 1)
(check-equal? (((my-if fals) 1) 2) 2)


