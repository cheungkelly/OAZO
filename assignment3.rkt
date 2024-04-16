#lang typed/racket
(require typed/rackunit)


;; ExprC ::= num
;;        | {+ ExprC ExprC}
;;        | {- ExprC ExprC}
;;        | {* ExprC ExprC}
;;        | {/ ExprC ExprC}
;;        | {id ExprC}
;;        | {ifleq0? ExprC ExprC ExprC}
;;        | id
;; Define the abstract syntax for expressions
(define-type ExprC
  (U NumC
     BinopC
     Ifleq0C
     AppC
     IdC))

(struct NumC ([n : Real]) #:transparent)
(struct BinopC ([op : Symbol]
                [l : ExprC]
                [r : ExprC]) #:transparent)
(struct PlusC ([l : ExprC]
              [r : ExprC]) #:transparent)
(struct MultC ([l : ExprC]
              [r : ExprC]) #:transparent)
(struct AppC ([fun : Symbol]
              [arg : ExprC]) #:transparent)
(struct Ifleq0C ([test : ExprC]
                 [then : ExprC]
                 [else : ExprC]) #:transparent)
(struct IdC ([name : Symbol]) #:transparent)

;; <fundef> ::= {fun {IdC IdC} : ExprC}
(struct FundefC ([name : Symbol]
                 [arg : (Listof Symbol)]
                 [body : ExprC]) #:transparent)

;; Parses an s-expression into an ExprC
(: parse (Sexp -> ExprC))
(define (parse s)
  (match s
    [(list '+ x y) (BinopC '+ (parse x) (parse y))]
    [(list '- x y) (BinopC '- (parse x) (parse y))]
    [(list '* x y) (BinopC '* (parse x) (parse y))]
    [(list '/ x y) (BinopC '/ (parse x) (parse y))]
    [(list 'ifleq0? test then else) (Ifleq0C (parse test) (parse then) (parse else))]
    ;[(list id arg) (AppC id (parse arg))]
    [(list (? symbol? fun) arg) (AppC fun (parse arg))]
    [(? real? n) (NumC n)]
    [(? symbol? id) (IdC id)]
    [_ (error "Unrecognized expression")]))

;; Parses a function definition s-expression
(: parse-fundef (Sexp -> FundefC))
(define (parse-fundef [s : Sexp]) : FundefC
  (match s
    [(list 'fun (list (? symbol? name) (? symbol? arg1) arg ...) ': body)
     (FundefC name (cons arg1 (cast arg (Listof Symbol))) (parse body))]
    [other (error 'parse-fundef "OAZO Func Syntax Error ~e" other)]))



;; Parses a program s-expression into a list of FundefC
(: parse-prog (Sexp -> (Listof FundefC)))
(define (parse-prog s)
  (match s
    [(list rest ...) (map parse-fundef rest)]
    [_ (error "Invalid program format")]))

;; Evaluates an expression in the context of a list of function definitions

(define (interp [exp : ExprC] [fundefs : (Listof FundefC)]) : Real
  (match exp
    [(NumC n) n]
    [(BinopC op l r)
     (define left-val (interp l fundefs))
     (define right-val (interp r fundefs))
     (match op
       ['+ (+ left-val right-val)]
       ['- (- left-val right-val)]
       ['* (* left-val right-val)]
       ['/ (if (= right-val 0) (error "Division by zero") (/ left-val right-val))]
       [else (error "Unsupported binary operator" op)])]
    [(Ifleq0C test then else)
     (define test-val (interp test fundefs))
     (if (<= test-val 0) (interp then fundefs) (interp else fundefs))]
    [(AppC func arg)
     (match (findf (lambda (f) (equal? (FundefC-name f) func)) fundefs)
       [(? FundefC f) (interp (FundefC-body f) (interp arg fundefs) fundefs)]
       [else (error "Function not found: ~a" func)])]
    [(IdC name) (error "Unbound variable encountered: ~a" name)]
    [else (error "Unrecognized expression")]))





;; Interprets the function named 'main' from the list of function definitions
(: interp-fns ((Listof FundefC) -> Real))
(define (interp-fns funs)
  (let ([main-fn (findf (lambda (f) (eq? (FundefC-name f) 'main)) funs)])
    (if main-fn
        (interp (FundefC-body main-fn) funs)
        (error "Main function not found"))))

;; Top-level interpreter that combines parsing and evaluation
(: top-interp (Sexp -> Real))
(define (top-interp fun-sexps)
  (interp-fns (parse-prog fun-sexps)))




