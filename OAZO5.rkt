#lang typed/racket
(require typed/rackunit)

;; Fully implemented OAZO5

;; ExprC ::= num
;;        | id
;;        | String
;;        | {if Expr then Expr else Expr}
;;        | {let [id <- Expr] ... Expr}
;;        | {anon {id ...} Expr}
;;        | {Expr Expr ...}

(define-type ExprC (U NumC IdC StrC IfC AnonC AppC))
(struct NumC ([n : Real]) #:transparent)
(struct IdC ([param : Symbol]) #:transparent)
(struct StrC ([str : String]) #:transparent)
(struct IfC ([test : ExprC] [then : ExprC] [else : ExprC]) #:transparent)
(struct AnonC ([args : (Listof Symbol)] [body : ExprC]) #:transparent)
(struct AppC ([fun : ExprC] [args : (Listof ExprC)]) #:transparent)

;; each name association in the environment is called a binding
(struct Binding ((name : Symbol) (val : Value)) #:transparent)
(define-type Env (Listof Binding))

;; Value ::= NumV
;;        |  CloV
;;        |  StrV
;;        |  BoolV
;;        |  PrimV

(define-type Value (U BoolV NumV StrV CloV PrimV))
(struct BoolV([n : Boolean]) #:transparent)
(struct NumV([n : Real]) #:transparent)
(struct StrV([n : String]) #:transparent)
(struct CloV([args : (Listof Symbol)] [body : ExprC] [env : Env]) #:transparent)
(struct PrimV ([op : Symbol]) #:transparent) 


;; —————————————————————————————————
;; implementation


;; gives meaning to the OAZO language
;; parameters: exp : ExprC, env : Env
;; returns: Value
(define (interp [e : ExprC] [env : Env]) : Value
  (match e
    [(NumC n) (NumV n)]
    [(IdC name) (lookup name env)]
    [(AnonC args body) (CloV args body env)]
    [(AppC f a) (define fd (interp f env))
                (define interped-args (map (lambda ([arg : ExprC]) (interp arg env)) a))
                (match fd
                  [(CloV args body env)
                   (if (equal? (length (CloV-args fd)) (length interped-args))
                       (interp (CloV-body fd)
                               (append
                                (map (lambda (param arg) (Binding param arg)) (CloV-args fd) interped-args)
                                (CloV-env fd)))
                       (error 'interp "OAZO Wrong Number of Arguments for Closure"))]
                  [(PrimV op) (if (equal? (length interped-args) 2)
                                  (num-op op (first interped-args) (first (rest interped-args)))
                                  (if (and (equal? 'error op) (first interped-args))
                                      (error 'interp "user-error: ~e" (serialize (first interped-args)))
                                      (error 'interp "OAZO Wrong Number of Arguments for PrimV function")))]
                  [other (error 'interp "OAZO Not a function for AppC: ~e" fd)])]
    [(IfC test then else)
     (define test-result (interp test env))
     (match test-result
       [(BoolV b) (if b (interp then env) (interp else env))]
       [other (error 'interp "OAZO IfC test expr is not a BoolV")])]
    [(StrC n) (StrV n)]))


;; looks up symbol in environment and returns its Value equivalent
;; parameters: for : Symbol, env : Env
;; returns: Value
(define (lookup [for : Symbol] [env : Env]) : Value
  (match env
    ['() (error 'lookup "OAZO name not found: ~e" for)]
    [(cons (Binding name val) r) (cond
                    [(symbol=? for name) val]
                    [else (lookup for r)])]))

;; top level environment
(define top-env
  (list
    (Binding 'true (BoolV #t))
    (Binding 'false (BoolV #f))
    (Binding '+ (PrimV '+))
    (Binding '- (PrimV '-))
    (Binding '* (PrimV '*))
    (Binding '/ (PrimV '/))
    (Binding '<= (PrimV '<=))
    (Binding 'equal? (PrimV 'equal?))
    (Binding 'error (PrimV 'error))))


;; wrapping numeric answers in appropriate numeric answer constructor and performs comparisons given the <= or equal?
;; parameters: op : Symbol, l : Value, r : Value
;; returns: Value
(define (num-op [op : Symbol] [l : Value] [r : Value]) : Value
  (cond
    [(equal? op 'equal?) (equal?-helper l r)] 
    [(and (NumV? l) (NumV? r)) 
     (match op
       ['+ (NumV (+ (NumV-n l) (NumV-n r)))]
       ['- (NumV (- (NumV-n l) (NumV-n r)))]
       ['* (NumV (* (NumV-n l) (NumV-n r)))]
       ['/ (if (= (NumV-n r) 0) 
               (error 'num-op "OAZO Division by zero") 
               (NumV (/ (NumV-n l) (NumV-n r))))]
       ['<= (BoolV (<= (NumV-n l) (NumV-n r)))]
       [else (error 'num-op "OAZO Invalid operation with PrimV")])] 
    [else (error 'num-op "OAZO Invalid operation")])) 


(define (equal?-helper [l : Value] [r : Value]) : Value
  (match (list l r)
    [(list (NumV n1) (NumV n2)) (BoolV (equal? n1 n2))]
    [(list (BoolV b1) (BoolV b2)) (BoolV (equal? b1 b2))]
    [(list (StrV s1) (StrV s2)) (BoolV (equal? s1 s2))]
    [(list (CloV _ _ _) (CloV _ _ _)) (BoolV #f)]
    [(list (PrimV _) (PrimV _)) (BoolV #f)]
    [else (BoolV #f)]))


;; accepts any OAZO5 value and returns a string
;; parameter: args : Value
;; returns: String
(define (serialize [args : Value]) : String
  (match args
    [(NumV n) (format "~v" n)] 
    [(BoolV b) (if b "true" "false")] 
    [(StrV n) (format "~v" n)] 
    [(CloV args body env) "#<procedure>"] 
    [(PrimV n) "#<primop>"])) 


;; parses an expression (including desugaring)
;; parameter: s : Sexp
;; returns: ExprC
(define (parse [s : Sexp]) : ExprC
  (match s
    [(? real? n) (NumC n)]
    [(? symbol? id) (if (not (invalid-id? id)) (IdC id) (error 'parse "OAZO Invalid Id Syntax for IdC"))]
    [(? string? str) (StrC str)]
    [(list 'if test 'then then-branch 'else else-branch)
     (IfC (parse test) (parse then-branch) (parse else-branch))]
    [(list 'let (list id '<- expr) ... body)
     (if (and (not (ormap (lambda (a) (invalid-id? a)) (cast id (Listof Symbol))))
              ((lambda (lst) (equal? lst (remove-duplicates lst))) (cast id (Listof Symbol))))
         (AppC (AnonC (cast id (Listof Symbol)) (parse body))
               (map (lambda (arg) (parse arg)) (cast expr (Listof Sexp))))
         (error 'parse "OAZO Invalid Id Syntax for let"))]
    [(list 'anon (list (? symbol? args) ...) ': body)
     (if (and (not (ormap (lambda (a) (invalid-id? a)) (cast args (Listof Symbol))))
              ((lambda (lst) (equal? lst (remove-duplicates lst))) (cast args (Listof Symbol))))
         (AnonC (cast args (Listof Symbol)) (parse body))
         (error 'parse "OAZO Invalid Id Syntax for AnonC"))]
    [(list id expr ...) (if (empty? expr) (AppC (parse id) '()) (AppC (parse id) (map parse expr)))]
    [other (error 'parse "OAZO Unrecognized expression: ~e" other)]))


;; checks if symbol is an invalid id
;; parameter: op : Symbol
;; returns: Boolean
(define (invalid-id? [op : Symbol]) : Boolean
  (match op
    ['if #t]
    ['then #t]
    ['else #t]
    ['let #t]
    ['anon #t] 
    [': #t]
    ['<- #t]
    [other #f]))


(define (top-interp [s : Sexp]) : String
  (serialize (interp (parse s) top-env)))


;; —————————————————————————————————
;; tests

(check-equal? (invalid-id? 'then) #t)
(check-equal? (invalid-id? 'else) #t)

(check-equal? (serialize (NumV 36)) "36")
(check-equal? (serialize (BoolV #t)) "true")
(check-equal? (serialize (BoolV #f)) "false")
(check-equal? (serialize (StrV "hello")) "\"hello\"")
(check-equal? (serialize (CloV (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) top-env)) "#<procedure>")
(check-equal? (serialize (PrimV '+)) "#<primop>")

(check-equal? (num-op '+ (NumV 2) (NumV 3)) (NumV 5))
(check-equal? (num-op '/ (NumV 20) (NumV 2)) (NumV 10))
(check-equal? (num-op '* (NumV 5) (NumV 5)) (NumV 25))
(check-equal? (num-op '- (NumV 127) (NumV 126)) (NumV 1))
(check-equal? (num-op '<= (NumV 127) (NumV 300)) (BoolV #t))
(check-equal? (num-op '<= (NumV 500) (NumV 300)) (BoolV #f))
(check-equal? (num-op 'equal? (NumV 127) (NumV 127)) (BoolV #t))
(check-equal? (num-op 'equal? (NumV 127) (NumV 1)) (BoolV #f))
(check-equal? (num-op 'equal? (StrV "127") (StrV "hello")) (BoolV #f))
(check-equal? (num-op 'equal? (StrV "127") (StrV "127")) (BoolV #t))
(check-equal? (num-op 'equal? (BoolV #t) (BoolV #t)) (BoolV #t))
(check-equal? (num-op 'equal? (BoolV #f) (BoolV #t)) (BoolV #f))
(check-equal? (num-op 'equal? (PrimV '+) (PrimV '-)) (BoolV #f))
(check-equal? (num-op 'equal? (BoolV #f) (PrimV '-)) (BoolV #f))
(check-exn #rx"OAZO Division by zero" (lambda () (num-op '/ (NumV 10) (NumV 0))))

(check-equal? (parse '{{anon {x y} : {+ x y}} 5 7})
              (AppC (AnonC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) (list (NumC 5) (NumC 7))))
(check-equal? (parse '{anon {x} : {+}}) (AnonC '(x) (AppC (IdC '+) '())))
(check-equal? (parse '{{anon {z y} : {+ z y}} {+ 9 14} 98}) (parse '{let [z <- {+ 9 14}] [y <- 98] {+ z y}}))
(check-equal? (parse '{if (<= 2 1) then 0 else 1}) (IfC (AppC (IdC '<=) (list (NumC 2) (NumC 1))) (NumC 0) (NumC 1)))
(check-exn #rx"OAZO Invalid Id Syntax for AnonC" (lambda () (parse '{anon {x x} : {+ x x}})))
(check-exn #rx"OAZO Invalid Id Syntax for IdC" (lambda () (parse '{anon {x} : {: x 1}})))

(check-equal? (interp (NumC 1) top-env) (NumV 1))
(check-equal? (interp (IdC '+) top-env) (PrimV '+))
(check-equal? (interp (AnonC '(x y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y)))) top-env)
              (CloV (list 'x 'y) (AppC (IdC '+) (list (IdC 'x) (IdC 'y))) top-env))
(check-equal? (interp (StrC "hello!") top-env) (StrV "hello!"))
(check-equal? (interp (IfC (AppC (IdC '<=) (list (NumC 2) (NumC 1))) (NumC 0) (NumC 1)) top-env) (NumV 1))
(check-exn #rx"OAZO IfC test expr is not a BoolV" (lambda () (interp (IfC (IdC '+) (NumC 1) (NumC 3)) top-env)))
(check-exn #rx"OAZO Wrong Number of Arguments for PrimV function"
           (lambda () (interp (AppC (AnonC '(x y)
                                           (AppC (IdC '+) (list (IdC 'x) (IdC 'y) (NumC 2))))
                                    (list (NumC 5) (NumC 7))) top-env)))

(check-equal? (top-interp '{{anon {compose add1} :
                                  {{anon {add2} : {add2 99}}
                                   {compose add1 add1}}}
                            {anon {f g} : {anon {a} : {f {g a}}}}
                            {anon {x} : {+ x 1}}}) "101")
(check-equal? (top-interp '(if true then (if false then 10 else 20) else 30)) "20")
(check-exn #rx"OAZO IfC test expr is not a BoolV" (lambda () (top-interp '(if 5 then 10 else 20))))
(check-equal? (top-interp "hello world") "\"hello world\"")
(check-exn #rx"lookup: OAZO name not found: 'x" (lambda () (top-interp '(x 1 2))))
(check-exn #rx"interp: OAZO Not a function for AppC" (lambda () (top-interp '((let [a <- 5] a) 1 2))))
(check-exn #rx"OAZO Invalid Id Syntax for let" (lambda () (top-interp '(let [let <- 30] let))))
(check-exn #rx"OAZO Invalid Id Syntax for let" (lambda ()
                                                 (check-exn #rx"parse: OAZO Unrecognized expression"
                                                            (lambda () (top-interp '())))
                                                 (check-exn #rx"parse: OAZO Invalid Id Syntax for IdC"
           (lambda () (top-interp '(if 1 2 3))))
(check-equal?(top-interp '(let [x <- 5] x))"5")
(check-equal? (top-interp '(let [x <- 5] (let [y <- 3] (+ x y)))) "8")
(check-exn #rx"OAZO Invalid Id Syntax for IdC"
           (lambda () (top-interp '(<- 5 10))))(top-interp '(let [anon <- 60] anon))))
(check-equal? (top-interp '{let [fact <- {anon {self n} :
                                               {if {<= n 0} then 1 else {* n {self self {- n 1}}}}}]
                             {fact fact 4}}) "24")
(check-equal? (top-interp '{let [fact <- {anon {self n} :
                                               {if {<= n 0} then 1 else {* n {self self {- n 1}}}}}]
                             {fact fact 7}}) "5040")
(check-equal? (top-interp '{let [fact <- {anon {self n} :
                                               {if {<= n 0}
                                                   then 1
                                                   else {* n {self self {- n 1}}}}}]
                             {fact fact 12}}) "479001600")
(check-equal? (top-interp '{let [f <- {anon {x} : {+ {* x x} {+ {* 4 x} 4}}}] {f 7}}) "81")
(check-exn #rx"OAZO name not found"
           (lambda () (top-interp '{let [fact <- {anon {n} :
                                                       {if {<= n 0} then 1 else {* n {fact {- 1 n}}}}}] {fact 4}})))
(check-exn #rx"user-error:" (lambda ()
                              (top-interp '{let [fact <- {anon {self n} :
                                                               {if {<= n 0}
                                                                   then (error "error here")
                                                                   else {* n {self self {- n 1}}}}}] {fact fact 4}})))
(check-exn #rx"OAZO Wrong Number of Arguments for Closure" (lambda () (top-interp '{{anon {} : 9} 7})))
(check-exn #rx"OAZO Invalid operation" (lambda () (top-interp '{+ + +})))

(check-equal? (num-op '+ (NumV 3) (NumV 2)) (NumV 5))  ; Addition
(check-equal? (num-op '- (NumV 5) (NumV 2)) (NumV 3))  ; Subtraction
(check-equal? (num-op '* (NumV 3) (NumV 2)) (NumV 6))  ; Multiplication
(check-equal? (num-op '/ (NumV 6) (NumV 2)) (NumV 3))  ; Division
(check-equal? (num-op '<= (NumV 2) (NumV 3)) (BoolV #t))  ; Less than or equal (true)
(check-equal? (num-op '<= (NumV 3) (NumV 2)) (BoolV #f))  ; Less than or equal (false)
(check-equal? (num-op 'equal? (NumV 3) (NumV 3)) (BoolV #t))  ; Numbers equal
(check-equal? (num-op 'equal? (BoolV #t) (BoolV #t)) (BoolV #t))  ; Booleans equal
(check-equal? (num-op 'equal? (StrV "test") (StrV "test")) (BoolV #t))  ; Strings equal
(check-equal? (num-op 'equal? (NumV 3) (NumV 4)) (BoolV #f))  ; Numbers not equal
(check-equal? (num-op 'equal? (BoolV #f) (BoolV #t)) (BoolV #f))  ; Booleans not equal
(check-equal? (num-op 'equal? (StrV "test1") (StrV "test2")) (BoolV #f))  ; Strings not equal

(check-exn #rx"OAZO Division by zero" (lambda () (num-op '/ (NumV 6) (NumV 0))))
(check-exn #rx"OAZO Invalid operation" (lambda () (num-op 'mod (NumV 6) (NumV 3))))
(check-exn #rx"OAZO Invalid operation" (lambda () (num-op '+ (BoolV #t) (NumV 3))))
(check-equal? (equal?-helper (CloV '(x) (NumC 1) empty) (CloV '(y) (NumC 2) empty))(BoolV #f))
(check-equal? (equal?-helper (PrimV '+) (PrimV '-))(BoolV #f))
(check-equal? (equal?-helper (CloV '(x) (NumC 1) empty) (PrimV '+))(BoolV #f))
(check-equal? (equal?-helper (NumV 1) (BoolV #t))(BoolV #f))