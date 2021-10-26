#lang simply-scheme

; 3.1

(* (+ 3 4) 5)
(+ 3 (* 4 5))

#|

3.2

(+ 3 (* 4 5) (- 10 4)) : 3 little people
 
(+ (* (- (/ 8 2) 1) 5) 2) : 4 little people
 
(* (+ (- 3 (/ 4 2))
      (sin (* 3 2))
      (- 8 (sqrt 5)))
   (- (/ 2 3)
      4)) : 10 little people

3.3

2
1
2

3.4

Amelia specialises in subtraction, takes 4 and 7 and returns -3 to Bertha
Andrea specialises in subtraction, takes 3 and 5 and returns -2 to Beatrice
Bertha specialises in multiplication, takes a 3 and -3 from Amelia and returns -9 to Carol
Beatrice specialises in subtraction, takes an 8 and -2 from Andrea and returns a 10 to Carol
Carol specialises in addition, takes -9 from Bertha and 10 from Beatrice and returns 1 to Alonso

3.5

(sqrt (+ 6 (* 5 2)))
(sqrt (+ 6 10     ))
(sqrt  16          )
4

(+ (+ (+ 1 2) 3) 4)
(+ (+ 3       3) 4)
(+  6            4)
10

3.6

No

3.7
1/3 - Fraction

3.8
(word)
(sentence)
(+)
(-)
(*)
(/)
(and)
(or)
(>)
(<)
(>=)
(<=)
(=)

|#

; 3.9

10
(+ (* 4 2) 2)
(* (+ 1 1) (- 7 2))
(+ (* 4 2) (- 9 8) 1)
(first (butfirst '(5 10 3 4 5)))
(* (+ 1 1) (- 7 2))
(sqrt (* 4 (expt 5 2)))
(word (- 99999 99998) (* 214254523 0))
(+ (+ (+ (+ (+ (+ (+ (+ (+ 1 1) 1) 1) 1) 1) 1) 1) 1) 1)