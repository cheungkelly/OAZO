#lang typed/racket
(require typed/rackunit)



;; Q1
(: rev-str-app (-> (Listof String) String))
(define (rev-str-app los)
  ; Purpose: To concatenate elements of a list of strings in reverse order.
  ; Type Contract: (listof string) -> string

  ; List Template
  ; (define (fn-for-los los)
  ;   (cond
  ;     [(empty? los) ...]
  ;     [else
  ;       (... (first los)
  ;            (fn-for-los (rest los)))]))

  ; Implementation
  (cond
    [(empty? los) ""]
    [else (string-append (rev-str-app (rest los)) (first los))]))


  (check-equal? (rev-str-app '("ball" "juice" "frog")) "frogjuiceball")
  (check-equal? (rev-str-app '()) "")
  (check-equal? (rev-str-app '("a" "b" "c")) "cba")


;;do match

;; Q2
;; The type of `rev-str-app` is to be (Listof String) -> String.
;; This makes sense because the function is designed to take a list of strings
;; and concatenate them into a single string in reverse order.

;; + Operator Explanation:
;; The type of `+` might be quite long and complex, for example:
;; (-> Real Real) or (-> Integer Integer) or ...
;; The reason for this complexity is that `+` is a polymorphic function in Typed Racket.
;; It can operate on various numeric types (e.g., integers, real numbers, complex numbers),
;; and its type signature reflects all these possible combinations.
;; Each combination of numeric types that `+` can operate on is represented in its type.


;; Q3

;; Define structs for each type of bicycle with a type annotation for the num-wheels field
(define-struct: Trek ([num-wheels : Number]))
(define-struct: Bianchi ([num-wheels : Number]))
(define-struct: Gunnar ([num-wheels : Number]))

;; Define a type 'Bicycle' that can be either a Trek, a Bianchi, or a Gunnar
(define-type Bicycle (U Trek Bianchi Gunnar))




;; Q5
;; Function: only-treks
;; Purpose: Consumes a list of bicycles and returns a list containing only the Treks.
;; Type: (Listof Bicycle) -> (Listof Trek)
(define (only-treks [bicycles : (Listof Bicycle)]) : (Listof Trek)
  (cond
    [(empty? bicycles) '()]  
    [else
     (let ([first-bike (first bicycles)]
           [rest-bikes (rest bicycles)])
       (if (Trek? first-bike)
           (cons first-bike (only-treks rest-bikes)) 
           (only-treks rest-bikes)))]))

;; Define the test bicycle instances
(define trek1 (make-Trek 2))
(define trek2 (make-Trek 3))
(define bianchi1 (make-Bianchi 4))
(define gunnar1 (make-Gunnar 5))

(define mixed-bicycles (list trek1 bianchi1 gunnar1 trek2))
(define no-trek-bicycles (list bianchi1 gunnar1))

;; Helper function to extract num-wheels from a list of Trek bikes
(define (get-num-wheels-list [bikes : (Listof Trek)]) : (Listof Number)
  (map Trek-num-wheels bikes))


(check-equal? (get-num-wheels-list (only-treks mixed-bicycles))
              (get-num-wheels-list (list trek1 trek2))
              "Test with mixed bicycle types: Should return only Treks")


(check-equal? (only-treks no-trek-bicycles) 
              '()
              "Test with no Trek bicycles: Should return an empty list")



;; Q6
;; Function: only-bianchis
;; Purpose: Consumes a list of bicycles and returns a list containing only the Bianchis.
;; Type: (Listof Bicycle) -> (Listof Bianchi)
(define (only-bianchis [bicycles : (Listof Bicycle)]) : (Listof Bianchi)
  (cond
    [(empty? bicycles) '()]
    [(Bianchi? (first bicycles))
     (cons (first bicycles) (only-bianchis (rest bicycles)))]
    [else (only-bianchis (rest bicycles))]))



;; More define
(define bianchi2 (make-Bianchi 5))
(define no-bianchi-bicycles (list trek1 gunnar1))

(check-equal? (map Bianchi-num-wheels (only-bianchis mixed-bicycles))
              (list 4)
              "Test with mixed bicycle types: Should return only Bianchis")

(check-equal? (only-bianchis no-bianchi-bicycles) 
              '()
              "Test with no Bianchi bicycles: Should return an empty list")



;; Q7
(define (onlyThese [bicycles : (Listof Bicycle)] [predicate : (-> Bicycle Boolean)]) : (Listof Bicycle)
  (cond
    [(empty? bicycles) '()]
    [else
     (if (predicate (first bicycles))
         (cons (first bicycles) (onlyThese (rest bicycles) predicate))
         (onlyThese (rest bicycles) predicate))]))

;tests
(check-equal? (length (onlyThese mixed-bicycles Trek?))
              2)
(check-equal? (length (onlyThese mixed-bicycles Gunnar?))
              1)

;; Q8
(: my-append (All [a] (Listof a) (Listof a) -> (Listof a)))
(define (my-append lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [else (cons (first lst1) (my-append (rest lst1) lst2))]))

(my-append '(a b c) '(d e f))


;; Q9
(: my-take (All [a] (Listof a) Integer -> (Listof a)))
(define (my-take lst n)
  (cond
    [(or (empty? lst) (<= n 0)) empty]
    [else (cons (first lst) (my-take (rest lst) (- n 1)))]))


(my-take '(1 2 3 4 5) 3) ;'(1 2 3)
(my-take '(1 2 3 4 5) 0) ;'()
(my-take '(a) 1) ;'(a)