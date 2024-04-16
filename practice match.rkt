#lang typed/racket
(require typed/rackunit)

(define (double [nums : (Listof Real)]) : (Listof Real)
  (match nums
    ['() '()]
    [(cons f r)(cons (* 2 f)(double r))]))


(check-equal? (double '())'())
(check-equal? (double '(10 20 30))'(20 40 60))
(check-equal? (double '(1.5 2.5 3.5))'(3.0 5.0 7.0))

(define (f [x : Integer]) : String
           (match x
             [0 "its zero"]
             [14 "not zero"]
             [(? even?) "its even"]
             [other (~v other)]))

(define (f2 [lst : (Listof Any)]) : String
  (match lst
    ['() "empty"]
    [(list l 2) "the list (1 2)"]
    [(cons (? real? x)r)(format "~v is real value"x)]))



(define-type ExprC
  (U NumC PlusC MultC))

(struct NumC ([n : Real]) #:transparent)
(struct PlusC ([l : ExprC][r : ExprC])#:transparent)
(struct MultC ([l : ExprC][r : ExprC])#:transparent)

(define (parse [code : Sexp]): ExprC
  (match code
    [(? real? x)(NumC x)]
    [(list '* left right)(MultC(parse left)(parse right))]
    [(list '+ left right)(PlusC(parse left)(parse right))]
    [other (error 'parse "expected valid syntax error, got ~e"other)]
))

(define (interp [ast : ExprC]) : Real
  (match ast
    [(NumC n)n]
    [(PlusC l r)(+(interp l)(interp r))]
    [(MultC l r)(*(interp l)(interp r))]))

(check-equal? (interp (NumC 4))4)