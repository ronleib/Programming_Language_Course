#lang pl
#|
----------------------------------- Q1.1 -----------------------------------

<SE>::= <num>
      | <nuber>
      | <cher>
      | <stg>

These specifications suffer from being ambiguous,
We want to get rid of this being ambiguous,
so that there is a single (= deterministic) way to derive all expressions.
There is a standard way to resolve that — we add another non-terminal to the definition,
and make it so that each rule can continue to exactly one of its alternatives.
For example, What I did with the numbers

<num>::= 0|1|2|3|4|5|6|7|8|9

<cher>::= #/<num>

we either restrict the way derivations can happen or we come up with new non-terminals to force a deterministic derivation trees.
As an example of restricting derivations, we look at the current grammar:

<nuber>::= <num> + <nuber>
         | <num>

<chers>::= <cher> + <cher>
         | <cher>

more example we can force them to appear in parentheses:

<stg>::= ""
       | "<nuber>"
       | (string <chers>)
       | (string-append  <stg> <stg>)
       | (string-append  <stg> <stg> <stg>)
       | (string-insert  <stg> <cher> <nuber>)
       | (number->string <SE>)
       | (string-length  <stglong>) 


<stglong>::= "<nuber>"


----------------------------------- Q1.2 -----------------------------------

1)( string #\1 #\2 #\4 ) ->>> (string <chers>) ->>> (stg <cher> <cher>) ->>> (stg <cher> <cher> <cher>) ->>> (stg #\1 #\2 #\4 )

2)( string-append ( string #\1 #\2 #\4 ) "12" ) ->>> (string-append <stg> <stg>) ->>> (string-append (string <chers>) "<nuber>") ->>> (string-append (stg <cher> + <cher>) "<nuber>")
 ->>> (string-append (stg <cher> <cher> <cher>) "12" ) ->>> (string-append (stg #\1 #\2 #\4 ) "12" )

3)( number->string 156879 ) ->>> (number->string <SE>)  ->>> "<nuber>"   ->>> "156879"

4)( number->string ( string-length "0033344" ) )  ->>> ( number->string <SE> ) ->>> ( number->string (string-length  <stglong>)) ->>> ( number->string (string-length <stglong>))
->>> ( number->string (string-length  "0033344")

|#

;----------------------------------- Q2 -----------------------------------

;Function is responsible to aggregate the sum of squares in input list for each value in the list, 
;And at the end returning of sum (Number)

(: sum-of-squares : (Listof Number) -> Number)
 (define (sum-of-squares list)
  (foldl + 0 (map sq list)) 
 )


;Function is responsible to do square operation on a number.

(: sq : Number -> Number)
 (define (sq num)
   (* num num)
 )

;tests sq

(test (sq 0) => 0)
(test (sq 2) => 4)
(test (sq -2) => 4)

;tests sum-of-squares

(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(0 1 2 3)) => 14)
(test (sum-of-squares '(0 -1 -2 -3)) => 14)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(-2 -2 -2)) => 12)




;----------------------------------- Q3.1 -----------------------------------
#|
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if power < (last (argsL))
        (+ accum (* (first (argsL)) (expt x power)) (createPolynomial (rest argsL)))
        (accum))
    (: polyX : Number -> Number)
    (define (polyX x)
      fill in→)
    fill in→)




  (: createPolynomial : (Listof Number) -> (Number -> Number))
  (define (createPolynomial coeffs)
    (: poly : (Listof Number) Number Integer Number -> Number)
    (define (poly argsL x power accum)
      (if (null? argsL)
          (polyX (x)))
          ((+1 power) createPolynomial (rest argsL)))
      (: polyX : Number -> Number)
      (define (polyX x)
        (+ accum (* (first (argsL)) (expt x power)))
      polyX (accum))








    (: createPolynomial : (Listof Number) -> (Number -> Number))
    (define (createPolynomial coeffs)
      (: poly : (Listof Number) Number Integer Number -> Number)
      (define (poly argsL x power accum)
        (if (power < (last(argsL)))
            ((poly (rest(polyX)) x (+1 power) accum)
            (+ accum (* (first (argsL)) (expt x power))))
            (accum) )
        (: polyX : Number -> Number)
        (define (polyX x)
          (poly polyX x 0 0)))
      polyX)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
 (: poly : (Listof Number) Number Integer Number ->
Number)
 (define (poly argsL x power accum)
 (if (null? argsL) accum  ;; stop condicion - then, return accum until there
   (poly(rest argsL) x (+ power 1) (+ accum (* (first argsL) (expt x power)))))) ;; each time, call the function Recursively with +1 for power, sum for  accum and Calculation of the function
 (: polyX : Number -> Number)
 (define (polyX x)
   (poly coeffs x 0 0)) ;; stating point
  polyX)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? (rest(argsL)))
        (+ accum (+ (poly (rest(argsL)) x (+ 1 power) accum) (* (first(argsL)) (expt x power)) ))
;        ((poly (rest(argsL)) x (+ 1 power) accum)
;         (+ accum (* (first(argsL)) (expt x power)))
        (accum)
        ))
    (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
polyX)

  
;> (createPolynomial '(1 2 4 2))-: (Number -> Number)

(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5
(expt 0 3))))
(test (p2345 4) => 
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)


|#

;----------------------------------- Q3.2.1 -----------------------------------
#|
 The grammar:
 <PLANG> ::=AEs
            |AE

 <AEs> ::= <AE> | <AE> <AEs>
 <AE> ::=<Number>
 |#
(: createPolynomial : (Listof Number) -> (Number -> Number))
(define (createPolynomial coeffs)
  (: poly : (Listof Number) Number Integer Number -> Number)
  (define (poly argsL x power accum)
    (if (null? argsL) accum
        (poly(rest argsL) x (+ 1 power) (+ (* (first argsL) (expt x power)) accum))))
;        ((poly (rest(argsL)) x (+ 1 power) accum)
;         (+ accum (* (first(argsL)) (expt x power)))
    (: polyX : Number -> Number)
  (define (polyX x)
    (poly coeffs x 0 0))
polyX)

  
;> (createPolynomial '(1 2 4 2))-: (Number -> Number)

(define p2345 (createPolynomial '(2 3 4 5)))
(test (p2345 0) =>
 (+ (* 2 (expt 0 0)) (* 3 (expt 0 1)) (* 4 (expt 0 2)) (* 5
(expt 0 3))))
(test (p2345 4) => 
 (+ (* 2 (expt 4 0)) (* 3 (expt 4 1)) (* 4 (expt 4 2)) (* 5
(expt 4 3))))
(test (p2345 11) => (+ (* 2 (expt 11 0)) (* 3 (expt 11 1)) (* 4
(expt 11 2)) (* 5 (expt 11 3))))
(define p536 (createPolynomial '(5 3 6)))
(test (p536 11) => (+ (* 5 (expt 11 0)) (* 3 (expt 11 1)) (* 6
(expt 11 2))))
(define p_0 (createPolynomial '()))
(test (p_0 4) => 0)

;----------------------------------- Q3.2.2 -----------------------------------

#|
(define-type PLANG
 [Poly (Listof AE) error])
 (define-type AE
 [Num Number]
 [Add AE AE]
 [Sub AE AE]
 [Mul AE AE]
 [Div AE AE])
 (: parse-sexpr : Sexpr -> AE)
 ;; to convert s-expressions into AEs
 (define (parse-sexpr sexpr)
 (match sexpr
 [(number: n) (Num n)]
 [(list '+ lhs rhs) (Add (parse-sexpr lhs)
 (parse-sexpr rhs))]
 [(list '- lhs rhs) (Sub (parse-sexpr lhs) 
 (parse-sexpr rhs))]
 [(list '* lhs rhs) (Mul (parse-sexpr lhs)
 (parse-sexpr rhs))]
 [(list '/ lhs rhs) (Div (parse-sexpr lhs)
 (parse-sexpr rhs))]
[else (error 'parse-sexpr "bad syntax in ~s"
 sexpr)]))

 (: parse : String -> PLANG)
 ;; parses a string containing a PLANG expression
 ;to a PLANG AST
;The function will return us Poly because it is where we want it
 (define (parse str)
 (let ([code (string->sexpr str)])
 Poly))
(test (parse "{{poly 1 2 3} {1 2 3}}")
 => (Poly (list (Num 1) (Num 2) (Num 3))
 (list (Num 1) (Num 2) (Num 3))))
(test (parse "{{poly } {1 2} }")
 =error> "parse: at least one coefficient is
 required in ((poly) (1 2))")
(test (parse "{{poly 1 2} {} }")
 =error> "parse: at least one point is
 required in ((poly 1 2) ())")



;----------------------------------- Q3.2.3 -----------------------------------


;; evaluates AE expressions to numbers
 (define (eval expr)
 (cases expr
 [(Num n) n]
 [(Add l r) (+ (eval l) (eval r))]
 [(Sub l r) (- (eval l) (eval r))]
 [(Mul l r) (* (eval l) (eval r))]
 [(Div l r) (/ (eval l) (eval r))]))

;The function returns us AE ..........................................................................

(: eval-poly : PLANG -> AE )
 (define (eval-poly p-expr)
 parse-sexpr (p-expr))
 (: run : String -> (Listof Number))
 ;; evaluate a FLANG program contained in a string
 (define (run str)
 (eval-poly (parse str)))

|#
