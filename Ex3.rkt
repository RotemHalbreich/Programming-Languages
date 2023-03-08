#lang pl

;;---------- By Ilan Teitelbaum 208117978   &   Rotem Halbreich 311549364 ------------

#|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUESTION 1: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   <AE> ::= <num>
          | { <AE> <AE> + }
          | { <AE> <AE> - }
          | { <AE> <AE> * }
          | { <AE> <AE> / }
          | { <AE> <AE> power }
          | { <AE> <AE> sqr }

In this question we're changing a bit the syntax of the language by extending it
with an addition of two operators: power & sqr and also change it to be in a posfix form.

it took us about 45 minutes
|#

;;Extended AE definition
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE]
  [Pow AE AE]
  [Sqr AE])


;;Extended parses s-expressions to AE
(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l r '+) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l r '-) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l r '*) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l r '/) (Div (parse-sexpr l)(parse-sexpr r))]
    [(list l r 'power) (Pow (parse-sexpr l)(parse-sexpr r))]
    [(list n 'sqr) (Sqr (parse-sexpr n))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]
    )
  )
 

;; Parses a string with AE expression to AE
(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code))
  )


;; consumes an AE and computes the corresponding number, defining Sqr to be (num*num)
(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num num) num]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (/ (eval l) (eval r))]
    [(Pow l r) (power (eval l) (eval r))]
    [(Sqr num) (* (eval num) (eval num))]
    )
  )


;; Power method that calculates x^y for number x and integer y
(: power : Number Number -> Number)
(define (power x y)
  (cond
    [(not (integer? y)) (error 'eval "power expects an integer power, got ~s" y)]
    [(> y 0) (* x (power x (- y 1)))]
    [else 1]
    )
  )


(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run code)
  (eval (parse code))
  )


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(test (parse "{+ 1 2 3 4}") =error> "bad syntax")
(test (parse "{* 1 2 3 4}") =error> "bad syntax")
(test (parse "{- 1 2 3 4}") =error> "bad syntax")
(test (run "3") => 3) 
(test (run "{3 4 +}") => 7) 
(test (run "{{3 4 -} 7 +}") => 6) 
(test (run "{{3 4 power} 7 +}") => 88)
(test (run "{{2 3 power} 5 +}") => 13)
(test (run "{{4 2 /} 7 +}") => 9) 
(test (run "{{3 4 *} 3 +}") => 15)
(test (run "{{3 0 power} 7 +}") => 8)
(test (run "{{2 4 power} {5 sqr} +}") => 41) 
(test (run "{{2 4/5 power} {5 sqr} +}") =error> "eval: power expects an integer power, got")


#|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUESTION 2: ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
~~~~~~~~~~~~~~~~~~~~~~~~~~~ 2.a Parser for the LE language ~~~~~~~~~~~~~~~~~~~~~~~~~~~

In this question we're using the BNF logic from the privious assignment and we'll
use it to complete the full interpeter.

This question took us about 5-6 hours.
|#


;; LE abstract syntax trees 
(define-type LE = (U LIST ATOM))


;; LIST abstract syntax trees 
(define-type LIST
  [NullTree]
  [Append (Listof LIST)]
  [Cons LE LIST]
  [Lst (Listof LE)])


;; ATOM abstract syntax trees 
(define-type ATOM 
  [NumTree Number]
  [Sym Symbol]
  )


(: parse-sexpr->LEs : (Listof Sexpr) -> (Listof LE))
;; converts a list of s-expressions into a list of LEs 
(define (parse-sexpr->LEs sexprs) 
   (map parse-sexprLE sexprs)
  )


(: parse-sexpr->LISTs : (Listof Sexpr) -> (Listof LIST))
;; converts a list of s-expressions into a list of LISTs
(define (parse-sexpr->LISTs sexprs) 
   (map parse-sexpr->LIST sexprs)
  )


(: parse-sexpr->LIST : Sexpr -> LIST)
;; converts s-expression into a LIST
(define (parse-sexpr->LIST sexpr) 
   (let ([ast (parse-sexprLE sexpr)]) 
   (if (LIST? ast) ast 
     (error 'parsesexprLE "expected LIST; got ~s" ast))
     )
  )


(: parse-sexprLE : Sexpr -> LE)
;; to convert s-expressions into LEs
(define (parse-sexprLE sexpr) 
  (match sexpr 
    [(number: num) (NumTree num)]
    ['null (NullTree)] 
    [(symbol: s) (Sym s)]
    [(cons 'append rest) (Append (parse-sexpr->LISTs rest))]          ;; Append
    [(list 'cons l r) (Cons (parse-sexprLE l) (parse-sexpr->LIST r))] ;; Cons
    [(cons 'list rest) (Lst (parse-sexpr->LEs rest))]                 ;; List
    [else (error 'parse-sexprLE "bad syntax in ~s" sexpr)])) 


(: parseLE : String -> LE)
;; parses a string containing a LE expression to a LE AST
(define (parseLE str)
(parse-sexprLE (string->sexpr str))
  )


;;~~~~~~~~~~~~~~~~~~~~~~~~~~ 2.a Evaluator for the LE language ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


(: eval-append-args : (Listof LE) -> (Listof (Listof Any)))
;; evaluates LE expressions by reducing them to lists
(define (eval-append-args exprs) 
  (if (null? exprs) null 
      (let ([fst-val (evalLE (first exprs))]) 
        (if (list? fst-val) 
            (cons fst-val (eval-append-args (rest exprs))) 
            (error 'evalLE "append argument: expected List got ~s" fst-val))
        )
      )
  )


(: evalLE : LE -> Any)
;; evaluates LE expressions by reducing them to numbers 
(define (evalLE expr) 
  (if (LIST? expr) 
      (cases expr
        [(NullTree) null]
        [(Append lst) (apply append (eval-append-args lst))]
        [(Cons l r) (let ([x (evalLE r)])
         (cond
           [(list? x) (cons (evalLE l) x)]
           [else (error 'evalLE "append argument: expected List got ~s" x)]))]
        [(Lst lst) (map evalLE lst)])
      
      (cases expr 
        [(NumTree n) n]
        [(Sym s) s])
      )
  )


(: runLE : String -> Any)
;; evaluate an WAE program contained in a string 
(define (runLE str) 
  (evalLE (parseLE str)))



;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ TESTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
(test (runLE "null") => null) 
(test (runLE "12") => 12)
(test (runLE "boo") => 'boo)
(test (runLE "{cons 1 {cons two null}}") => '(1 two)) 
(test (runLE "{list 1 2 3}") => '(1 2 3)) 
(test (runLE "{list {cons}}") =error> "parse-sexprLE: bad syntax in (cons)") 
(test (runLE "{list {cons 2 1}}") =error> "parsesexprLE: expected LIST; got")
(test (parseLE "null") => (NullTree)) 
(test (parseLE "2") => (NumTree 2))
(test (parseLE "Rotem") => (Sym 'Rotem))
(test (parseLE "{cons 3 {cons Ilan null}}") => (Cons (NumTree 3) (Cons (Sym 'Ilan) (NullTree)))) 
(test (parseLE "{list 1 2 3}") => (Lst (list (NumTree 1) (NumTree 2) (NumTree 3))))
(test (parseLE "{append {list 1 2 3} {list 4 5}}") => (Append (list (Lst (list (NumTree 1) (NumTree 2) (NumTree 3))) (Lst (list (NumTree 4) (NumTree 5))))))
(test (parseLE "{list {cons}}") =error> "parse-sexprLE: bad syntax in (cons)") 
(test (parseLE "{list {cons 2 1}}") =error> "parsesexprLE: expected LIST; got") 
(test (eval-append-args (list (Lst (list (NumTree 2) (NumTree 5) (NumTree 9))) (Lst (list (NumTree 1) (NumTree 3))))) => (list (list 2 5 9) (list 1 3)))


