#lang typed/racket
(require typed/rackunit)

;; 2.3.3
;; total-profit : number -> number
(define (total-profit [customers : Number])
  (- (* 5 customers) (+ 20 (* 0.5 customers))))

;; test
(check-equal? (total-profit 5) 2.5)
(check-equal? (total-profit 10) 25.0)
(check-equal? (total-profit 100) 430.0)


;; 3.3.3
;; area-cylinder : Real Real -> Real
;; to determine the area of a cylinder
(: area-cylinder (-> Real Real Real))
(define (area-cylinder radius height)
  (+ (* (circumference-circle radius) height)
     (area-circle radius)
     (area-circle radius)))


;; area-circle : Real -> Real
;; to determine the area of a circle
(: area-circle (-> Real Real))
(define (area-circle radius)
  (* pi radius radius))

;; circumference-circle : Real -> Real
;; to determine the circumference of a circle
(: circumference-circle (-> Real Real))
(define (circumference-circle radius)
  (* radius 2 pi))

;; tests
(check-equal? (area-cylinder 1 1) (+ (* (circumference-circle 1) 1) 
                                     (area-circle 1) 
                                     (area-circle 1)))
(check-equal? (area-cylinder 2 3) (+ (* (circumference-circle 2) 3) 
                                     (area-circle 2) 
                                     (area-circle 2)))



;; 2.2 
(struct Card-Trick ([decks : Number] [volunteers : Integer]))
(struct Guillotine ([realism : Number] [has-tiger? : Boolean]))

(define (trick-minutes trick)
  (match trick
    [(Card-Trick decks volunteers)
     (* decks (expt 2 volunteers))]
    [(Guillotine realism has-tiger?)
     (if has-tiger? 20 10)]))

(check-equal? (trick-minutes (Card-Trick 1 0)) 1 "Card-Trick with 1 deck and 0 volunteers")
(check-equal? (trick-minutes (Card-Trick 1 1)) 2 "Card-Trick with 1 deck and 1 volunteer")
(check-equal? (trick-minutes (Card-Trick 2 2)) 8 "Card-Trick with 2 decks and 2 volunteers")
(check-equal? (trick-minutes (Guillotine 5 #f)) 10 "Guillotine trick without a tiger")
(check-equal? (trick-minutes (Guillotine 7 #t)) 20 "Guillotine trick with a tiger")


;; 2.3

(define-type Polynomial
  (U Linear Quadratic))

(struct Linear ([A : Number] [B : Number])#:transparent)
(struct Quadratic ([A : Number] [B : Number] [C : Number])#:transparent)

(define (interp poly [x : Number]) 
  (match poly
    [(Linear A B)
     (+ (* A x) B)]
    [(Quadratic A B C)
     (+ (* A x x) (* B x) C)]))

(check-equal? (interp (Linear 2 3) 4) 11 "Linear: 2x + 3 where x = 4")
(check-equal? (interp (Quadratic 1 2 1) 2) 9 "Quadratic: x^2 + 2x + 1 where x = 2")
(check-equal? (interp (Quadratic 0 3 4) 5) 19 "Quadratic: 0x^2 + 3x + 4 where x = 5")


;; 2.4

(define (derivative poly) : Polynomial
  (match poly
    [(struct Linear (A B))
     (Linear 0 A)]
    [(struct Quadratic (A B C))
     (if (= A 0)
         (Linear 0 B)
         (Linear (* 2 A) B))]))

(check-equal? (cast (derivative (Linear 3 2)) Any) (cast (Linear 0 3) Any) "Derivative of 3x + 2")
(check-equal? (cast (derivative (Quadratic 2 4 1)) Any) (cast (Linear 4 4) Any) "Derivative of 2x^2 + 4x + 1")
(check-equal? (cast (derivative (Quadratic 0 5 3)) Any) (cast (Linear 0 5) Any) "Derivative of 0x^2 + 5x + 3")
;; 2.5
(define-type BTree
  (U Leaf Node))

(struct Leaf () #:transparent)
(struct Node ([symbol : Symbol] [left : BTree] [right : BTree])#:transparent)


(define example1 (Leaf))
(define example2 (Node 'A (Leaf) (Leaf)))
(define example3 (Node 'B (Node 'C (Leaf) (Leaf)) (Node 'D (Leaf) (Leaf))))



;; 2.6

(define (max-right-skew-depth [tree : BTree]) : Real
  (define (helper [tree : BTree] [current-depth : Real]) : Real
    (match tree
      [(Leaf) current-depth]
      [(Node _ left right)
       (max (+ (helper left current-depth) 1.0) ; left branch adds 1.0 points
            (+ (helper right current-depth) 1.5))])) ; right branch adds 1.5 points
  (helper tree 0.0))


(define tree1 (Leaf)) ; A single leaf
(check-equal? (max-right-skew-depth tree1) 0.0)

(define tree2 (Node 'A (Leaf) (Leaf))) ; A simple tree with two leaves
(check-equal? (max-right-skew-depth tree2) 1.5)

(define tree3 (Node 'B (Node 'C (Leaf) (Leaf)) (Node 'D (Leaf) (Leaf)))) ; A more complex tree
(check-equal? (max-right-skew-depth tree3) 3.0)

;; 2.7
(: count-occurrences : (BTree Symbol -> Integer))
(define (count-occurrences tree symbol)
  (match tree
    [(Leaf) 0]
    [(Node s left right)
     (+ (if (eq? s symbol) 1 0)
        (count-occurrences left symbol)
        (count-occurrences right symbol))]))

(: contains-twice? : (BTree Symbol -> Boolean))
(define (contains-twice? tree symbol)
  (>= (count-occurrences tree symbol) 2))

(define test-tree1 (Node 'a (Node 'b (Leaf) (Leaf)) (Node 'a (Leaf) (Leaf))))
(define test-tree2 (Node 'x (Node 'y (Leaf) (Leaf)) (Node 'z (Leaf) (Leaf))))
(define test-tree3 (Node 'm (Node 'm (Node 'n (Leaf) (Leaf)) (Leaf)) (Leaf)))

(check-equal? (contains-twice? test-tree1 'a) #true)  ; Tree contains 'a' twice
(check-equal? (contains-twice? test-tree1 'b) #false) ; Tree contains 'b' only once
(check-equal? (contains-twice? test-tree2 'x) #false) ; Tree does not contain 'x' twice


;; 2.8
(: subst (-> BTree Symbol BTree BTree))
(define (subst source-tree target-symbol replacement-tree)
  (match source-tree
    [(Leaf) (Leaf)] ; Leaves are left unchanged
    [(Node symbol left right)
     (if (eq? symbol target-symbol)
         replacement-tree ; Replace the entire node with the replacement tree
         (Node symbol 
               (subst left target-symbol replacement-tree) ; Recursively apply subst to the left subtree
               (subst right target-symbol replacement-tree)))]))
;; tests
(define leaf (Leaf))
(define tree5 (Node 'a leaf leaf))
(define tree6 (Node 'a (Node 'b leaf leaf) (Node 'c leaf leaf)))
(define tree7 (Node 'b (Node 'a leaf leaf) (Node 'b leaf leaf)))
(define replacement-tree (Node 'x leaf leaf))

(check-equal? (subst tree5 'a replacement-tree) replacement-tree "Substitute root symbol")
(check-equal? (subst tree5 'z replacement-tree) tree5 "Substitute non-existent symbol")
(check-equal? (subst tree6 'b replacement-tree) (Node 'a replacement-tree (Node 'c leaf leaf)) )
(check-equal? (subst tree6 'b leaf) (Node 'a leaf (Node 'c leaf leaf)) "Substitute symbol with a leaf")
