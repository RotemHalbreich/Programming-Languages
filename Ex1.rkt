#lang pl

#|
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUESTION 1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
It took me more than 8 hours (because I had to learn the syntax of this programming language)
At first I've tried to write a min/max functions and after a little research I figured out
that there are min/max functions of Racket, then I ran some tests and had errors,
so I've decided to use the min/max function I've implemented in Q2B and use them here also.
|#
#|
HELP METHOD FOR ~ min&max ~ Returns the minimum number in a list
We get a list of numbers and a single number for comparing to each number in the list,
if the list is empty -> Return the single number
else if the first number of the list is smaller than the single number,
return the method without the first number and make the comparing number as the first number of the list
else return the method with the list without the first number and the single number from the start.
|#
(: minimum : (Listof Number) Number -> Number)
(define (minimum lst min)
  (cond [(null? lst) min]
        [(< (first lst) min) (minimum (rest lst) (first lst))]
        [else (minimum (rest lst) min)]
    ))

(test (minimum '(2 -5 1 5 4 5 99 3 2 0) -5) => -5)

#|
HELP METHOD FOR ~ min&max ~ Returns the maximum number in a list
We get a list of numbers and a single number for comparing to each number in the list,
if the list is empty -> Return the single number
else if the first number of the list is bigger than the single number,
return the method without the first number and make the comparing number as the first number of the list
else return the method with the list without the first number and the single number from the start.
|#
(: maximum : (Listof Number) Number -> Number)
(define (maximum lst max)
  (cond [(null? lst) max]
        [(> (first lst) max) (maximum (rest lst) (first lst))]
        [else (maximum (rest lst) max)]
    ))

(test (maximum '(2 -5 1 5 4 5 99 3 2 0) 99) => 99)

#|
Gets 5 numbers and returns a list with 2 numbers -> '(min-num max-num)
First I'm inserting the 5 numbers into a list the method 'list',
inside a 'let' method to define the list with the name 'lst',
at the end I'm calling both minimum & maximum (my helping functions) in a list method, 
with the new list and the first single number to get a new list such that -> '(min max)
|#
(: min&max : Number Number Number Number Number -> (Listof Number))
(define (min&max a b c d e)
  (let ([lst (list a b c d e)])
    (list (minimum lst a) (maximum lst a))
  ))

(test (min&max 2 3 2 7 5) => '(2 7))
(test (min&max 9 1 9 9 9) => '(1 9))
(test (min&max 1 2 3 4 5) => '(1 5))
(test (min&max 1 1 1 1 1) => '(1 1))
(test (min&max 10 1 9 9 -34) => '(-34 10))
(test (min&max 0 1 9 9 100) => '(0 100))
(test (min&max 0 -1 -7.6 9.9 -8.1) => '(-8.1 9.9))
(test (min&max -1 3.7 4.7 8 2) => '(-1 8))
(test (min&max -2 -4 -63 8.34 4) => '(-63 8.34))
(test (min&max 5 5 5 5 5) => '(5 5))
(test (min&max  0 0 -1 -23.56 0) => '(-23.56 0))


;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUESTION 2A ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;; It took me 45 minutes
;;At first I did not fully understand what is tail-recursion, so I used a regular recursion for
;; solving the question, then I've changed it so it took me extra 20 minutes.
#|
HELP METHOD FOR ~ sublist-numbers ~ (TAIL-RECURSION)
Gets a lists of multi-type elements and a list of numbers and returns a new list with only the numbers.
if the any list is null -> return the list of numbers, then I'm checking if the first element in the any list is of type number
if it is a number, I'm calling the method with the any list without the first number and
another list using cons for making a pair of the first element of any + num list.
else, returns the method with any list without the first element and num list.
|#
(: sublist-help : (Listof Any) (Listof Number) -> (Listof Number))
(define (sublist-help any num)
  (cond [(null? any) num]
        [(number? (first any)) (sublist-help (rest any) (cons (first any) num))]
        [else (sublist-help (rest any) num)]
   ))
#|
THE MAIN METHOD
Gets a list of multi-type elements and returns a new list with the prior list's numbers
if the list if null -> returns the list, else returns 'sublist-help' help function
with the list of any and an empty list of numbers.
at the end I'm filling the empty list of numbers with numbers from the any list and when
the list of any is completely empty, I'm returning the filled number list.
|#
(: sublist-numbers : (Listof Any) -> (Listof Number))
(define (sublist-numbers lst)
  (cond [(null? lst) lst]
        [else (sublist-help lst '())]
   ))

(test (sublist-numbers (list 'any "Benny" 10 'OP 8)) => '(8 10))
(test (sublist-numbers (list 'any "Benny" -9 'OP)) => '(-9))
(test (sublist-numbers (list 'any "Benny" -9 0 'OP)) => '(0 -9))
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '(any "Benny" OP (2 3))) => '())
(test (sublist-numbers '()) => '())
(test (sublist-numbers '(() ())) => '())


;;~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ QUESTION 2B ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
;;It took me more than 6 hours
;; I had to think about the helping methods.

#|
HELP METHOD FOR ~ insert-sublists ~
Gets a list of multi-type elements and returns a new list with the prior list's numbers.
To do that I'm using the method 'sublist-numbers' from question 2A to get a list of numbers only
then takes the new list of numbers and returns a new list with 2 numbers -> '(min-num max-num)
if the new list is null, returns null, else returns my minimum & maximum help methods (from QUESTION 1)
with the list without the first number and the single number becomes the first number of the list.
|#
(: inner-list : (Listof Any) -> (Listof Number))
(define (inner-list lst)
  (let ([lst-num (sublist-numbers lst)])
    (cond [(null? lst-num) '()]
          [else (list (minimum (rest lst-num) (first lst-num)) (maximum (rest lst-num) (first lst-num)))]
    )))

(test (inner-list '(2 -5 -56 5 L 4 100 5 "Benny" 6 'OP 99 3 2 0)) => '(-56 100))
(test (inner-list '(2 -5 1 5 L 4 5 "Benny" 6 'OP 99 3 2 0)) => '(-5 99))
(test (inner-list '(2 2)) => '(2 2))
(test (inner-list '(2)) => '(2 2))
(test (inner-list '('s 'R 's)) => '())

#|
MAIN METHOD:
Gets a list of multi-type lists and returns as a list of number lists,
each list consists from -> '(min max)
Example: '((any)(any)(any)) -> '((num)(num)(num))
if the list is null -> returns null list
else I'm using a cons function to make a pair of 'inner-list' with the first list of the any list
+ calling the method with all the lists in the big any list except for the first list.
|#
(: min&max-lists : (Listof(Listof Any)) -> (Listof(Listof Number)))
(define (min&max-lists lst)
  (cond [(null? lst) '()]
        [else (cons (inner-list (first lst)) (min&max-lists (rest lst)))]
   ))

(test (min&max-lists '((100 5 "Benny" 6 'OP 99 3 2 0) (14 "$" 'S) ())) => '((0 100) (14 14) ())) 
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (23)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(())) => '(()))
(test (min&max-lists '(() ())) => '(() ()))
(test (min&max-lists '(() (-6.8))) => '(() (-6.8 -6.8)))
(test (min&max-lists '(() ((-6.8) OS "ROTEM"))) => '(() ()))
(test (min&max-lists '((1 OS "Rotem") (3 6) (-2 7 8) (2))) => '((1 1) (3 6) (-2 8) (2 2)))
(test (min&max-lists '((0) (3 6) (-2 0 7 8) (2))) => '((0 0) (3 6) (-2 8) (2 2)))


#|
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Question 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Implement a keyed-stack data structure
It took me around 30 minutes because I had to search in lecture 3 how to implement a class
and use the Uninon (U) for the return output
|#

;; Define KeyStack
;; A constructor with an empty key stack and a Push method that gets a symbol, string and another KeyStack
(define-type KeyStack
  [EmptyKS]                      ; 1. Empty keyed-stack
  [Push Symbol String KeyStack]  ; 2. Push variant
  )

;; 3. Search method
;; This method gets a Symbol and a KeyStack and returns a Union between String and Boolean (false)
;; If the KeyStack is empty -> return false.
;; else, check if both keys are identical, if so return the value,
;; else, return the method with the first key and the second key stack.
(: search-stack : Symbol KeyStack -> (U String #f))
(define (search-stack key_1 stack_1)
  (cases stack_1
    [(EmptyKS) #f]
    [(Push key_2 value stack_2) (cond [(eq? key_1 key_2) value]
                                [else (search-stack key_1 stack_2)])]
  ))

;; 4. Pop method
;; This method gets a KeyStack and returns a Union between KeyStack and Boolean (false)
;; If the KeyStack is empty -> return false.
;; else -> pop the stack
(: pop-stack : KeyStack -> (U KeyStack #f))
(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push key value stack) stack]
  ))

(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)
(test (search-stack 'a (EmptyKS)) => #f)




;; Tests from the lecturer:
(test (min&max 5 4 3 2 1) => '(1 5))
(test (min&max 5 6 3 -5 -9) => '(-9 6))
(test (min&max 0 0 0 0 0) => '(0 0))
(test (min&max -5 -2 -7 -3 -3) => '(-7 -2))
(test (or (equal? (min&max -5 -2.5 -7 -3 -3) '(-7 -2.5)) (equal? (min&max -5 -2.5 -7 -3 -3) '(-7.0 -2.5))) => #t) 
(test (or (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8)) (equal? (min&max 1.5 8 3.3 1 0.2) '(0.2 8.0))) => #t)
(test (or (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(0 20)) (equal? (sublist-numbers (list 'adf "moshe" 0 20 'ds)) '(20 0))) => #t) 
(test (sublist-numbers '(any "Benny" OP (2 3))) => null)
(test (sublist-numbers '()) => null)
(test (or (equal? (sublist-numbers '(1 2 3 4 5 6)) '(6 5 4 3 2 1)) (equal? (sublist-numbers '(1 2 3 4 5 6)) '(1 2 3 4 5 6))) => #t) 
(test (or (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(1.5 8 3.3 1 0.2)) (equal? (sublist-numbers '(1.5 8 3.3 1 0.2)) '(0.2 1 3.3 8 1.5))) => #t) 
(test (sublist-numbers '(4 (fd)'((1 2 3)))) => '(4))
(test (sublist-numbers '(any "Benny" OP (2 3))) => '())
(test (or (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(2 3)) (equal? (sublist-numbers '(any "Benny" OP 2 3)) '(3 2))) => #t)
(test (min&max-lists '((2 5 1 5 L) () (4 5 6 7.8 3 2 1) ()))  => '((1 5) () (1 7.8) ()))
(test (min&max-lists '((1) ()))  => '((1 1) ()))
(test (min&max-lists '(("fFAGG" 9 'DEF 2.5) (1.9 "Fd" 4 4.1 4/3) ( 0 "Fd" 0 -1 2/3)))  => '((2.5 9) (4/3 4.1) (-1 2/3)))
(test (min&max-lists '(("fFAGG" 'DEF 3.11) (0)))  => '((3.11 3.11) (0 0)))
(test (min&max-lists '()) => '())
(test (min&max-lists '((any "Benny" 10 OP 8) (any "Benny" OP (2 3)))) => '((8 10) ()))
(test (min&max-lists '((2 5 1 5 L) (4 5 6 7 3 2 1) ())) => '((1 5) (1 7) ()))
(test (min&max-lists '(() () ())) => '(() () ()))
(test (min&max-lists '((2))) => '((2 2)))
(test (min&max-lists '((2) ("test" (1 2)))) => '((2 2) ()))
(test (min&max-lists '((2 2) ('q) (-1 -1 1 1))) => '((2 2) () (-1 1)))
(test (min&max-lists '(() ("test") ('a))) => '(() () ()))
(test (min&max-lists '((E S S A L) (a A Z E R T 1) ())) => '(() (1 1) ()))
(test (EmptyKS) => (EmptyKS))
(test (Push 'a "a" (Push 'A "A" (EmptyKS))) => (Push 'a "a" (Push 'A "A" (EmptyKS))) )
(test (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))) =>(Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS)))))
(test (search-stack 'aaa (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => "AA")
(test (search-stack 'f (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => "f")
(test (search-stack 'g (Push 'a "A" (Push 'b "B" (Push 'c "c" (Push 'd "d" (Push 'e "e" (Push 'f "f" (EmptyKS)))))))) => #f)
(test (search-stack 'E (Push 'EEE "EEE" (Push 'V "V" (Push 'Q "QQ" (EmptyKS))))) => #f)
(test (search-stack 'E (EmptyKS)) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "Bisli" (Push 'a "Bamba"(EmptyKS))))) => "Bisli")
(test (search-stack 'cb (Push 'cb "LIAV" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "LIAV")
(test (pop-stack (Push 'aaa "AA" (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))) => (Push 'bbb "BBB" (Push 'aaa "AAA" (EmptyKS))))
(test (pop-stack (Push 'A "A" (EmptyKS))) => (EmptyKS))
(test (pop-stack (EmptyKS)) => #f)
