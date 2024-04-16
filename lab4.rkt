#lang racket
(require rackunit)
;; lab4

;; takes a number ’a’ and
;; returns a function that takes a number ’b’ and returns a+b.
;; (number -> (number -> number))
(define (curried-add a)
  (lambda (b) (+ b a)))

(check-equal? ((curried-add 1) 2) 3)
(check-equal? ((curried-add 5) 4) 9)
(check-equal? ((curried-add 0) 1) 1)

;; takes a function of two arguments f, and
;; produces a function that we’ll call M.
;; (All (a b c) ((a b -> c) -> (a -> (b -> c))))
(define (curry2 f)
  (lambda (m) (lambda (n) (f m n))))

(check-equal? (((curry2 +) 1) 2) 3)
(check-equal? (((curry2 +) 5) 4) 9)
(check-equal? (((curry2 +) 0) 1) 1)

;; takes a function of three arguments, and
;; produces a function that takes one argument and
;; produces a function that takes one argument and
;; produces a function that takes one argument and
;; produces the result of calling the input function on the three given arguments
;; (All (a b c d) ((a b c -> d) -> (a -> (b -> (c -> d)))))
(define (curry3 f)
  (lambda (a) (lambda (b) (lambda (c) (f a b c)))))

(check-equal? ((((curry3 +) 1) 2) 3) 6)
(check-equal? ((((curry3 +) 5) 4) 9) 18)
(check-equal? ((((curry3 +) 0) 1) 1) 2)

;; consumes a list and a symbol and
;; returns true exactly when the symbol occurs in the list.
(define (contains? l s)
  (cond
    [(empty? l) #f]
    [(equal? (first l) s) #t]
    [else (contains? (rest l) s)]))

(check-equal? (contains? '(a b c) 'a) #t)
(check-equal? (contains? '(a b c) 'd) #f)
(check-equal? (contains? '() 'a) #f)

;; consumes a source list of symbols and a list of query symbols, and
;; returns a list of booleans indicating for the corresponding element of the query list whether it occurs in the source list.
(define (in-list-many? l q) (map ((curry2 contains?) l) q))

(check-equal? (in-list-many? '(a b c) '(a c)) '(#t #t))
(check-equal? (in-list-many? '(a b c) '(x y z)) '(#f #f #f))
(check-equal? (in-list-many? '(a b c) '(a x c)) '(#t #f #t))








