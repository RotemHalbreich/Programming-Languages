#lang pl 02 
#| Question 1:
It took us 45 minutes to solve this question

<BNF> ->
~~~~~~
  <LE> ::= {1}  <num>
           {2}| '<sym>
           {3}| ( append <LIST-LE> (...) )
           {4}| <LIST-LE>

 
 <LIST-LE> ::= {5}  ( <LIST-LE> <LE> (...) )
               {6}| ( cons <LE> <LIST-LE> )
               {7}| ( list <LE> (...) )
               {8}| null
          

_______________________________________________________________________
(cons 797 (cons (append (cons 'Ilan null) (list 'Teit 'elbaum)) null))
_______________________________________________________________________
BNF Test with steps:
=>(i):
~~~~~~
=>(4) <LIST-LE>
=>(6) ( cons <LE> <LIST-LE> )
=>(1) ( cons 797 <LIST-LE> )
=>(6) ( cons 797 ( cons <LE> <LIST-LE> ) )
=>(3) ( cons 797 ( cons ( append <LIST-LE> <LIST-LE> ) <LIST-LE> ) )
=>(6) ( cons 797 ( cons ( append ( cons <LE> <LIST-LE> ) <LIST-LE> ) <LIST-LE> ) )
=>(2) ( cons 797 ( cons ( append ( cons 'Ilan <LIST-LE> ) <LIST-LE> ) <LIST-LE> ) )
=>(8) ( cons 797 ( cons ( append ( cons 'Ilan null ) <LIST-LE> ) <LIST-LE> ) )
=>(5) ( cons 797 ( cons ( append ( cons 'Ilan null ) ( <LIST-LE> <LE> <LE> ) ) <LIST-LE> ) )
=>(2) ( cons 797 ( cons ( append ( cons 'Ilan null ) ( <LIST-LE> 'Teit <LE> ) ) <LIST-LE> ) )
=>(2) ( cons 797 ( cons ( append ( cons 'Ilan null ) ( <LIST-LE> 'Teit 'elbaum ) ) <LIST-LE> ) )
=>(7) ( cons 797 ( cons ( append ( cons 'Ilan null ) ( list 'Teit 'elbaum ) ) <LIST-LE> ) )
=>(8) ( cons 797 ( cons ( append ( cons 'Ilan null ) ( list 'Teit 'elbaum ) ) null ) )
|#



#|~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Question 2.1~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
The AE grammer:

  <AE> ::= <num>
           | { <AE> + <AE> }
           | { <AE> - <AE> }
           | { <AE> * <AE> }
           | { <AE> / <AE> } 
|#
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

#|
Changing the original order that was given to an Infix order
Took us 1 minute
|#
(: parse-sexpr : Sexpr -> AE)
(define (parse-sexpr sxp)
  (match sxp
    [(number: n) (Num n)]
    [(list l '+ r) (Add (parse-sexpr l)(parse-sexpr r))]
    [(list l '- r) (Sub (parse-sexpr l)(parse-sexpr r))]
    [(list l '* r) (Mul (parse-sexpr l)(parse-sexpr r))]
    [(list l '/ r) (Div (parse-sexpr l)(parse-sexpr r))]
    [else (error 'parse-sexpr "bad syntax in ~s" sxp)]))


(: parse : String -> AE)
(define (parse code)
  (parse-sexpr (string->sexpr code)))


(test (parse "{ 3 + 4 }") => (Add (Num 3)(Num 4)))
(test (parse "3") => (Num 3))
(test (parse "{ {3 - 2} + 4 }") => (Add (Sub (Num 3)(Num 2))(Num 4)))
(test (parse "{+ 1 2 3 4}") =error> "bad syntax")


#|
The goal of parse:
Input:  string describing the program
Output: Abstract Syntax Tree (or an exception if the string is not a valid program)

Two main phases:
1. Read -- turn the string into a simple data structure (we will use the Racket type Sexpr).
2. Actual Parsing -- turn an Sexpr into an AST


Definition of the pl type Sexpr:
Basis -- any Number/Symbol is an Sexpr
General -- any list of Sexpr is an Sexpr
|#


;;Question 2.2
#|
Changing the eval code that was given by adding a new condition: if divided by 0 use OUR answer and return
999 which represents infinity instead of Racket error - "cannot divide by 0"

It took us 5 minutes
|#
(: eval : AE -> Number)
(define (eval exp)
  (cases exp
    [(Num n) n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r)
     (cond [(eq? (eval r) 0) 999]
           [else (/ (eval l) (eval r))]
           )]
    )
  )


(: run : String -> Number)
(define (run code)
  (eval (parse code)))


(test (run "{1 / 0}") => 999)
(test (run "{{1 / 0} + 1}") => 1000)
(test (run "{0 / 1}") => 0)
(test (run "{60 + 9}") => 69)
(test (run "{{3 / 3} + 2}") => 3)
(test (eval (Num 5)) => 5)
(test (eval (Add (Num 3) (Num 4))) => 7)
(test (eval (Add (Sub (Num 3) (Num 2)) (Num 4))) => 5)
(test (eval (parse "{3 + 4 }")) => 7)
(test (eval (parse "3")) => 3)
(test (eval (parse "{{3 - 2} +  4 }")) => 5)
(test (eval (parse "{1 + 2 + 3 4}")) =error> "bad syntax")
(test (eval (parse "{3 * {5 / 3} }")) => 5)
(test (run "{3 + 4 }") => 7)
(test (run "3") => 3)
(test (run "{ + {- 3 2}  4 }") =error> "bad syntax")
(test (run "{+ 1 2 3 4}") =error> "bad syntax")



;;~~~~~~~~~~~~~~~~~~~~~~~~~~~Question 3~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#|
Function gets a number and return the square of the number by multiplying the number by itself

Took us 1 minute
|#
(: pow : Number -> Number)
 (define (pow num)
   (* num num)
   )

#|
Function gets a list of numbers and uses first of all the 'map' function with the pow function
that we wrote and changes the list accordingly to the pow function, then uses 'foldl' function to
"fold" the list using the '+ symbol as argument and returns a sum of the list as a number

Took us between 30-40 minutes to understand how to use the 'foldl' method properly.
|#
(: sum-of-squares : (Listof Number) -> Number)
 (define (sum-of-squares lst)
   (foldl + 0 (map pow lst))
   )

(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(0)) => 0)
(test (sum-of-squares '(12)) => 144)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(0 1 3)) => 10)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(0.5 0.5 -0.5)) => 0.75)
(test (sum-of-squares '(1 0.5 -0.5)) => 1.5)
(test (pow 10) => 100)


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Question 4~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;Question 4.a:
#|
Defining a Leaf to hold a number
Defining a Node to hold 2 BINTREE objects

Took us 10 minutes
|#
(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE]
  )


;;Question 4.b:
#|
Checks if the tree is just a leaf, returns the Leaf after the given f(n) function
if its a Node call the function again in a recurisive way with the left side of the node and right side of the node

It took us about an hour
|#
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
 (define (tree-map f tree)
   (cases tree
     [(Leaf num) (Leaf (f num))]
     [(Node ln rn)(Node (tree-map f ln) (tree-map f rn))]
     )
   )  


(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map sub1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3)))) => (Node (Leaf 0) (Node (Leaf 1) (Leaf 2))))
(test (tree-map add1 (Node (Leaf 10) (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))) => (Node (Leaf 11) (Node (Leaf 2) (Node (Leaf 3) (Leaf 4)))))
(test (tree-map add1 (Node (Leaf 5) (Leaf 10))) => (Node (Leaf 6) (Leaf 11)))
