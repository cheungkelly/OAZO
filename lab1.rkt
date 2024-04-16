#lang typed/racket
(require typed/rackunit)


;; 15
(define (==> sunny friday)
  (or (not sunny) friday))
;;test
(check-equal? (==> #true #true) #true)   
(check-equal? (==> #true #false) #false) 
(check-equal? (==> #false #true) #true)  
(check-equal? (==> #false #false) #true)

;;19
(: string-insert (-> String Integer String))
(define (string-insert str i)
  (string-append (substring str 0 i) "_" (substring str i)))
;; test
(string-insert "hello" 2) ; => "he_llo"
(string-insert "" 0)     ; => "_"
(string-insert "test" 4) ; => "test_"

;;27
;;sample problem
(define base-attendees : Integer 120)
(define change-by-people : Integer 15)
(define price-change-interval : Real 0.10)
(define fixed-cost : Real 180)
(define variable-cost-per-person : Real 0.04)
(define base-ticket-price : Real 5.0)

(: attendees (-> Real Real))
(define (attendees ticket-price)
  (round (- base-attendees (* (/ (- ticket-price base-ticket-price) price-change-interval) change-by-people))))
;; ^use round because of float number

(: revenue (-> Real Real))
(define (revenue ticket-price)
  (* ticket-price (attendees ticket-price)))

(: cost (-> Real Real))
(define (cost ticket-price)
  (+ fixed-cost (* variable-cost-per-person (attendees ticket-price))))

(: profit (-> Real Real))
(define (profit ticket-price)
  (- (revenue ticket-price) (cost ticket-price)))

; Write test cases

(check-equal? (revenue 5.0) (* 5.0 120) "Test for revenue at base ticket price")

(check-equal? (cost 5.0) (+ fixed-cost (* variable-cost-per-person 120)) "Test for cost at base ticket price")
(check-equal? (cost 5.10) (+ fixed-cost (* variable-cost-per-person 105)) "Test for cost with increased ticket price")

(check-equal? (profit 5.0) (- (revenue 5.0) (cost 5.0)) "Test for profit at base ticket price")
(check-equal? (profit 5.10) (- (revenue 5.10) (cost 5.10)) "Test for profit with increased ticket price")

(check-equal? (attendees 5.0) 120.0 "Test for base number of attendees at base ticket price")
(check-equal? (attendees 5.10) 105.0 "Test for reduced number of attendees with increased ticket price")
(check-equal? (attendees 4.90) 135.0 "Test for increased number of attendees with decreased ticket price")


;; 4.4

;; Define the Desk structure
(define-type Furniture (U desk bookshelf))
(struct desk ([width : Real] [height : Real] [depth : Real]))

;; Define the Bookshelf structure
(struct bookshelf ([depth : Real] [num-shelves : Integer] [shelf-width : Real]))

;; Examples
(define example-desk (desk 50.0 30.0 20.0))
(define example-bookshelf (bookshelf 15.0 5 35.0))


;; Define the furniture-footprint function
(: furniture-footprint (-> Furniture Real))
(define (furniture-footprint furniture)
  (match furniture
    [(desk width height depth) (* width depth)]
    [(bookshelf depth num-shelves shelf-width) (* shelf-width depth)]))

;; Test cases for furniture-footprint
(check-equal? (furniture-footprint example-desk) (* 50.0 20.0) "Test footprint for desk")
(check-equal? (furniture-footprint example-bookshelf) (* 35.0 15.0) "Test footprint for bookshelf")
