#lang simply-scheme

#|

Boring Exercises

4.1

(define (ho-hum x y)
  (+ x (* 2 y))

(ho-hum 8 12)

(+ 8 (* 2 12))
(+ 8      24 )
32

4.2

(define (yawn x)
  (+ 3 (* x 2)))

(yawn (/ 8 2))

Doris the divider takes 8 and 2, divides them and passes 4 to
Matt the multiplier, who takes the 4 and 2 and mulitplies them and hands 8 to
Amanda the adder who takes 8 and adds 3 to it, handing 11 to
Yertle the ywner, who hands the 11 to Alonso

4.3

(define (f x y) (- y x))

f subtracts the second argument from the first.
(f 4 2)
2

(define (identity x) x)

identitiy returns the value passed as an argument
(identity 5)
5

(define (three x) 3)

three takes an argument but always returns the value 3
(three 123342)
3

(define (seven) 7)

seven takes no arguments and returns the value 7
(seven)
7

(define (magic n)
  (- (/ (+ (+ (* 3 n)
              13)
           (- n 1))
        4)
     3))

magic takes an argument, multiplies it by 3, adds it to itself minus 1, adds 13, divides the result by 4 and subtracts 3

|#


; Real Exercises

#| 4.4

(define (sphere-volume r)
  (* (/ 4 3) 3.141592654)
  (* r r r))

tries to return 2 expressions - needs to return the result of both expressions multiplied together

|#

(define (sphere-volume r)
  (* (/ 4 3) 3.141592654 (* r r r)))

(sphere-volume 30)

#|
(define (next x)
  (x + 1))

incorrect order of function and arguments in brackets
|#
(define (next x)
  (+ x 1))
(next 4)

#|
(define (square)
  (* x x))

no argument named in definition

|#

(define (square x)
  (* x x))

(square 4)

#|
(define (triangle-area triangle)
  (* 0.5 base height))

argument triangle not used, base and height not defined
|#
(define (triangle-area base height)
  (* 0.5 base height))
(triangle-area 10 10)

#|
(define (sum-of-squares (square x) (square y))
  (+ (square x) (square y)))

arguments cannot fave functions applied at the point of definition. The function calls should be moved into the body
|#

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 5 5)


; 4.5

(define (f-to-c temp)
  (* (/ 5 9) (- temp 32)))
(f-to-c 100)

(define (c-to-f temp)
  (+ (* (/ 9 5) temp) 32))

(c-to-f 37.8)

; 4.6

(define (fourth-v1 x)
  (* (* (* (* x) x) x) x))
(fourth-v1 2)

(define (fourth-v2 x)
  (square (square x)))

(fourth-v2 2)

; 4.7

(define (abs x)
  (sqrt (square x)))
(abs -20)

; 4.8

(define (scientific val exp)
  (* (expt 10 exp) val))

(scientific 7 3)
(scientific 42.0 -5)

; 4.9

(define (discount price percentage-off)
  (- price (* price (/ percentage-off 100))))

(discount 10 5.0)

; 4.10

(define (tip amount)
  (- (ceiling (+ amount (* amount 0.15))) amount))
(tip 19.98)
(tip 29.23)
(tip 7.54)
