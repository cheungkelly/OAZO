#lang typed/racket
(require typed/rackunit)

(provide file-interp)
 
(define-type Simple-Sexp
  (U Symbol Number String (Listof Simple-Sexp)))
 
(define-predicate sexp? Simple-Sexp)
 
(define (file-interp [f : Path-String])
  (define v (file->value f))
  (cond [(sexp? v) (top-interp v)]
        [else (error 'file-interp "expected file to contain s-expression, got: ~e" v)]))

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
                  [(PrimV op)
                   (if (member op '(+ - * / <= equal?))
                        (if (equal? (length interped-args) 2)
                           (num-op op (first interped-args) (first (rest interped-args)))
                           (error 'interp "OAZO Wrong Number of Arguments for operation: ~a" op))
                       (if (equal? 'error op)
                           (if (not (null? interped-args))
                               (error 'interp "OAZO user-error: ~e" (serialize (first interped-args)))
                               (error 'interp "OAZO error operation expects at least one argument"))
                           (match op
                             ['println 
                              (begin
                                (println-helper interped-args) 
                                (BoolV #t))]
                             ['read-num
                              (if (= (length interped-args) 0)
                                  (read-num-helper)
                                  (error 'interp "OAZO read-num expects no arguments"))]
                             ['seq (seq-helper interped-args)]
                             ['++ (concat-helper interped-args)]
                             [else (error 'interp "OAZO Unsupported Operation: ~a" op)])))]
                  [other (error 'interp "OAZO Not a function for AppC: ~e" fd)])]
    [(IfC test then else)
     (define test-result (interp test env))
     (match test-result
       [(BoolV b) (if b (interp then env) (interp else env))]
       [other (error 'interp "OAZO IfC test expr is not a BoolV")])]
    [(StrC n) (StrV n)]))


;; Iterate over each argument, ensuring it's treated as a Value for serialization
;; Print a space after each argument for separation
;; After printing all arguments, print a newline character to finish the line
;; Return BoolV #t to signify successful execution
(define (println-helper [args : (Listof Value)]) : Value
   (for-each (lambda ([arg : Value])
              (display (serialize arg))
              (display " ")) 
            args)
   (newline) 
  (BoolV #t))


;; prompt >
;; Attempt to convert the input to a number
;; Check if the conversion was successful and the result is a real number
;; Wrap the real number in NumV
(define (read-num-helper) : Value
  (display "> ")
  (define input (read-line))
  (if (eof-object? input)
      (error 'read-num-helper "OAZO expected number input.")      
      (let ([num (string->number input)])        
        (if (and num (real? num))  
            (NumV num)  
            (error 'read-num-helper "OAZO Input is not a real number")))))

;; If there's only one element left, it's the result.
;; Return the only or last element.
;; Recurse with the rest of the list.
(define (seq-helper [args : (Listof Value)]) : Value
  (cond
    [(empty? args) (error 'seq-helper "OAZO Empty sequence")]
    [(empty? (rest args)) (first args)]
    [else (seq-helper (rest args))]))


;; Convert each argument to its string representation,
;; join them into a single string, and
;; return StrV
(define (concat-helper [args : (Listof Value)]) : Value
  (define strings (map (lambda (a)
                         (match a
                           [(NumV a) (number->string a)]
                           [(StrV a) a]
                           [other (error 'concat-helper
                                         "OAZO Given value type not handled for concatenation: ~e" other)]))
                       args))
  (StrV (foldr string-append "" strings)))


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
    (Binding 'error (PrimV 'error))
    (Binding 'println (PrimV 'println))
    (Binding 'read-num (PrimV 'read-num))
    (Binding 'seq (PrimV 'seq))
    (Binding '++ (PrimV '++))))


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
(check-exn #rx"OAZO Wrong Number of Arguments"
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

(check-equal? (concat-helper (list (StrV "hi") (StrV "bye"))) (StrV "hibye"))
(check-equal? (concat-helper (list (StrV "hi"))) (StrV "hi"))
(check-equal? (concat-helper (list (StrV "hi") (NumV 2) (NumV 34))) (StrV "hi234"))
(check-exn #rx"OAZO Given value type not handled for concatenation:" (lambda () (concat-helper (list (BoolV #f)))))


;; fun program
#;{seq
 {let [health <- 100] [decrease-health <- {anon {h} : {- h 25}}]
    {seq {println "You're in the middle of a zombie apocalypse at Cal Poly! Quick! Your phone is at what percent?"}
   {let [percent <- {read-num}]
     {seq {println {++ "Oh, your phone is at " percent "%?"}}
          {if (<= percent 15)
              then {println "Wow, how unfortunate. Did you forget to charge?"}
              else {println "That's great! That amount might just last you through this whole game..."}}
          {println "Be fast! Someone's unlocking the classroom door! It sounds like your classmate!
                     Type 1 to open the door or 2 to lock it."}
          {let [pick <- {read-num}]
            {if {equal? pick 1}
                then {seq {println "Your classmate thanks you and is very grateful. But, they're kind
                                     of twitching strangely..."}
                          {println "They suddenly ask you to come over and see what they salvaged from
                                     the Cal Poly Bookstore."}
                          {println "You're starving. You are also conveniently placed near an open
                                     window for escape."}
                          {println {++ "HEALTH: " {decrease-health health}}}
                          {println "Type 1 to satiate your hunger or 2 to escape through the window."}
                          {let [pick2 <- {read-num}]
                            {if {equal? pick2 1}
                                then {seq {println "Not smart. They take that chance to bite you!"}
                                          {println "Your phone has no battery, and you are too weak to
                                                    get to the window. You succumb to the bite..."}
                                          {println {++ "HEALTH: " {decrease-health {decrease-health
                                                                                    {decrease-health health}}}}}
                                          {println "GAME OVER."}}
                                else {seq {println "You sprint to the window but stumble on your way out.
                                                    You scrape your knees in a rush."}
                                          {println {++ "HEALTH: " {decrease-health health}}}
                                          {println "You run and run but reach a dilemma. Type 1 to run
                                           out of campus to safety or 2 to find an outlet to charge your phone."}
                                          {let [pick3 <- {read-num}]
                                            {if {equal? pick3 1}
                                                then {seq {println "You live to see another day! But
                                                      at the cost of your friends who are still in Cal Poly..."}
                                                          {println "GAME END."}}
                                                else {seq {println "You attempt to find an outlet but
                                                                 someone attacks you! You never saw it coming."}
                                                          {println "You are brutually mauled by the
                                                                   zombie Cal Poly student, with nothing
                                                                  to defend yourself and weak from hunger."}
                                                          {println {++ "HEALTH: " {decrease-health
                                                                                   {decrease-health
                                                                                    {decrease-health health}}}}}
                                                          {println "Unfortunately you succumb to the
                                                                    wounds as you failed to fight them off."}
                                                          {println "GAME OVER."}}}}}}}}
                else {seq {println "You locked the door. You hear them yell and run away to try
                                    another classroom."}
                          {if {<= percent 15}
                              then {seq {println "Your phone is dead and there is no outlet. You
                                                  stay put and wait for help. But you end up waiting
                                                  forever... no one comes so you die!"}
                                        {println "GAME OVER."}}
                              else {seq {println "Your phone buzzes. It's your friend who's nearby!
                                                   They are very distressed. Type 1 to go look
                                                    for them or 2 to stay put."}
                                        {let [pick4 <- {read-num}]
                                          {if (equal? pick4 1)
                                              then {seq {println "The only way out is the window
                                                                   next to you, without the risk of
                                                                   running into the person you didn't let in."}
                                                        {println "You sprint to the window
                                                                   but stumble on your way out.
                                                                    You scrape your knees in a rush."}
                                                        {println {++ "HEALTH: " {decrease-health health}}}
                                                        {println "You find them intact and
                                                                 safe. Both of you have not been bitten!"}
                                                        {println "With the power of friendship and a not-dead
                                                 phone you both work together to escape Cal Poly and succeed."}
                                                        {println "GAME END."}}
                                              else {seq {println "You stay put and wait for help.
                                                   But you end up waiting forever... no one comes so you die!"}
                                                        {println "GAME END."}}}}}}}}}}}}}}
;; sample run
#;("You're in the middle of a zombie apocalypse at Cal Poly! Quick! Your phone is at what percent?" 
> 1
"Oh, your phone is at 1%?" 
"Wow, how unfortunate. Did you forget to charge?" 
"Be fast! Someone's unlocking the classroom door! It sounds like your classmate!
Type 1 to open the door or 2 to lock it." 
> 1
"Your classmate thanks you and is very grateful. But, they're kind of twitching strangely..." 
"They suddenly ask you to come over and see what they salvaged from the Cal Poly Bookstore." 
"You're starving. You are also conveniently placed near an open window for escape." 
"HEALTH: 75" 
"Type 1 to satiate your hunger or 2 to escape through the window." 
> 1
"Not smart. They take that chance to bite you!" 
"Your phone has no battery, and you are too weak to get to the window. You succumb to the bite..." 
"HEALTH: 25" 
"GAME OVER." 
)