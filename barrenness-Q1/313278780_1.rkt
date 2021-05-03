#lang pl


;;---------------------------------------q1----------------------------------

(: append5 : Char Char Char Char Char -> String)
(define(append5 a b c d e)
  (string a b c d e))

(test (append5 #\a #\s #\d #\f #\g) => "asdfg")
(test (append5 #\B #\B #\B #\B #\B) => "BBBBB")
(test (append5 #\a #\a #\a #\a #\A) => "aaaaA")


(: permute3 : Char Char Char -> (Listof String))
(define(permute3 a b c)
  (list (string a b c) (string a c b) (string b a c) (string b c a) (string c a b) (string c b a)))


(test (permute3 #\a #\s #\c) => '("asc" "acs" "sac" "sca" "cas" "csa"))
(test (permute3 #\a #\a #\a) => '("aaa" "aaa" "aaa" "aaa" "aaa" "aaa"))
(test (permute3 #\b #\c #\ ) => '("bc " "b c" "cb " "c b" " bc" " cb"))




;;---------------------------------------q2----------------------------------

#|
function count-3lists that consumes a list of lists (where the type of the elements in the inner
lists may be any type) and returns the number of inner lists (within the
wrapping list) that contain exactly 3 elements.
|#

(: count-3lists : (Listof(Listof Any)) -> Natural)
(define (count-3lists lst)
  (if(null? lst)
     0
     (if(equal? (length(first lst)) 3)
        (+ 1 (count-3lists(rest lst)))
        (count-3lists(rest lst)))))

(test (count-3lists '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)

#|
function count-3lists-tail that works the same as
count-3lists but now you to use tail-recursion (Name tail-recursion - helper) .
|#


(: count-3lists-tail : (Listof(Listof Any)) -> Natural)
(define (count-3lists-tail lst)
  (helper 0 lst))


(: helper : Natural (Listof(Listof Any)) -> Natural)
(define (helper n lst)
  (if(null? lst)
     n
     (if(equal? (length(first lst)) 3)
        (helper (+ 1 n) (rest lst))
        (helper n (rest lst)))))


(test (count-3lists-tail '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 3)



#|
function count-3listsRec that is similar to the
above count-3lists, however counts the number of lists of length 3
recursively in on all levels of nesting.
|#



#|
(: count-3listsRec : (Listof(Listof Any)) -> Natural)
(define (count-3listsRec lst)
  (if(null? lst)
     0
     (if(and andmap list? (first lst) (equal? (length(first lst)) 3))
        (+ 1 (+ (count-3listsRec(first lst)) (count-3listsRec(rest lst))))
        (if(andmap list? (first lst))
           (+(count-3listsRec(first lst))( count-3listsRec(rest lst)))
           (if(equal? (length(first lst)) 3)
              (+ 1 (count-3listsRec(rest lst)))
              (count-3listsRec(rest lst))
              )
           )
        )
     )
  )
         

(test (count-3listsRec '((1 3 4) (() (1 2 3)) ("tt" "Three" 7) (2 4 6 8) (1 2 3))) => 4)
(: count-3listsRec : (Listof(Listof Any)) -> Natural)
  (define (count-3listsRec lst)
       (if(null? lst)
         0
         (if(andmap list? (first lst))
            (if(equal? (length(first lst)) 3)
               (+ 1(+ (count-3listsRec(first lst))(count-3listsRec(rest lst)))
                  (+ (count-3listsRec(first lst))(count-3listsRec(rest lst)))))
            (if(equal? (length(first lst)) 3)
            (+ 1 (count-3listsRec(rest lst)))
                 (count-3listsRec(rest lst)))
           
         ))
    )

|#

;;---------------------------------------q3----------------------------------


#|

build keyed-stack data structure.
constructor:
EmptyKS - empty stack
Push    – operation should take as input a symbol (key),string (value) , KeyStack (stack)
|#
(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])

#|

operation search-stack – the search operation
input a symbol (key) and a keyed-stack and return the first
value that is keyed accordingly. If the key does not appear in the original stack, it should return a #f value.


|#

(: search-stack  : Symbol KeyStack -> (U String Boolean))
(define (search-stack key stack)
  (cases stack
    [(EmptyKS) #f]
    [(Push symbol string nexKS) (if (eq? key symbol) string (search-stack key nexKS))]
    )
  )


#|
operation pop-stack – the pop operation should take
as input a keyed-stack and return the keyed-stack without its first (keyed) value .
If the original stack was empty return a #f.
|#

(: pop-stack : KeyStack -> (U KeyStack Boolean))
(define (pop-stack stack)
  (cases stack
    [(EmptyKS) #f]
    [(Push symbol string nexKS) nexKS]
    )
  )

; Tests
(test (EmptyKS) => (EmptyKS))
(test (Push 'a "a" (Push 'b "ABC" (EmptyKS))) => (Push 'a "a" (Push 'b "ABC" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'c "CCC" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'c "CCC" (EmptyKS)))))
(test (search-stack 'a (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS))))) => "A")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'd "d" (EmptyKS))))) => #f)
(test (search-stack 'b (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "B")
(test (search-stack 'f (Push 'a "AAA" (Push 'b "B" (Push 'd "d" (EmptyKS))))) => #f)
(test (pop-stack (Push 'a "A" (Push 'b "B" (Push 'c "C" (EmptyKS))))) => (Push 'b "B" (Push 'c "C" (EmptyKS))))
(test (pop-stack (EmptyKS)) => #f)



;;---------------------------------------q4----------------------------------

(: is-odd? : Natural -> Boolean)
#|
The function accepts a natural number and returns true or false (if the number is even or odd)
|#

#|
functions works recursively,
if x is zero then x is returns false we return true if x is odd iff (x-1) is even which checked with is-even?
|#
(define (is-odd? x)
  (if (zero? x)
      false
      (is-even? (- x 1))))

(: is-even? : Natural -> Boolean)

#|
The function accepts a natural number and returns true or false (if the number is even or odd)
|#

#|
functions works recursively,
if x is zero then x is returns true we return true if x is odd iff (x-1) is even which checked with is-even?
|#

(define (is-even? x)
  (if (zero? x)
      true
      (is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))

#|
function that checks if every element of type A is satisfying function of type (A -> Boolean)
input lst of type A and checks if every element of type A -> Boolean
return true if every type in lst.
|#

(define (every? pred lst)
  (or (null? lst)
      (and (pred (first lst))
           (every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
#|
list of Natural numbers and pred function from (Netural -> Boolean) which checks if
on an input x, x is even. 
|#
(define (all-even? lst)
  (every? is-even? lst))


(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))

(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) ->
                  Boolean))
#|
function that check oll i in {1,2} if lst_i satisfying pred_i
|#
(define (every2? pred1 pred2 lst1 lst2)
  (or (null? lst1)
      (and (pred1 (first lst1))
           (pred2 (first lst2))
           (every2? pred1 pred2 (rest lst1) (rest lst2)))))
