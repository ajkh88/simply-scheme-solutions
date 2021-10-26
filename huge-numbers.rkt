#lang simply-scheme

(define (spell-num n)
  (cond ((eq? n 1) 'one)
        ((eq? n 2) 'two)
        ((eq? n 3) 'three)
        ((eq? n 4) 'four)
        ((eq? n 5) 'five)
        ((eq? n 6) 'six)
        ((eq? n 7) 'seven)
        ((eq? n 8) 'eight)
        ((eq? n 9) 'nine)
        ((eq? n 10) 'ten)
        ((eq? n 11) 'eleven)
        ((eq? n 12) 'twelve)
        ((eq? n 13) 'thirteen)
        ((eq? n 14) 'fourteen)
        ((eq? n 15) 'fifteen)
        ((eq? n 16) 'sixteen)
        ((eq? n 17) 'seventeen)
        ((eq? n 18) 'eighteen)
        ((eq? n 19) 'nineteen)
        ((eq? n 0) '())))

(define (spell-tens n)
  (item n '(#f twenty thirty forty fifty sixty seventy eighty ninety)))

(define (spell-triple-helper trip)
  (cond ((< trip 20) (spell-num trip))
        ((< trip 100) (se (spell-tens (quotient trip 10)) (spell-triple-helper (remainder trip 10))))
        ( else (se (spell-num (quotient trip 100)) 'hundred (spell-triple-helper (remainder trip 100))))))

(define (spell-triple trip)
  (cond ((< trip 20) (se (spell-num trip)))
        (else (spell-triple-helper trip))))


(define (get-order-of-magnitude num)
  (item num '(() thousand million billion trillion quadrillion quintillion
  sextillion septillion octillion nonillion decillion)))


(define (number-name-helper n order)
  (cond ((= n 0) '())
        ((= (remainder n 1000) 0)  (number-name-helper (quotient n 1000) (+ 1 order)))
        (else (se (number-name-helper (quotient n 1000) (+ 1 order)) (spell-triple (remainder n 1000)) (get-order-of-magnitude order)))))

(define (number-name n)
  (number-name-helper n 1))

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

(number-name 3)
(number-name 34)
(number-name 13)
(number-name 345)
(number-name 2352)
(number-name 34567)
(number-name 345678)
(number-name 10234)
(number-name 1000529)
(number-name 100)
(number-name 1000)
(number-name 10000)
(number-name 100000)
(number-name 1000000)
(number-name 10000000)
(number-name 100000000)
(number-name (factorial 20))



