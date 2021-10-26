#lang simply-scheme

#|

Boring Exercises

9.1

(lambda (x) (+ (* x 3) 4))
 #<procedure>

((lambda (x) (+ (* x 3) 4)) 10)
34

(every (lambda (wd) (word (last wd) (bl wd)))
         '(any time at all))
'(yan etim ta lal)

((lambda (x) (+ x 3)) 10 15)
#<arity mismatch>

|#
; 9.2

(define second (lambda (stuff) (first (bf stuff))))

(define make-adder (lambda (num) (lambda (x) (+ num x))))

(second 'abc)
((make-adder 3) 4)


#|

9.3
(define (let-it-be sent)
  (accumulate (lambda (x y) y) sent))

This procedure returns the last value in the sentence passed as an argument
The lambda will always return the second value, and this is applied along the entire collection until the end,
essentially returning the second value of the last pair, which is the last item.

|#


; Real Exercises

; 9.4

(define (who sent)
  (every (lambda (name) (se name sent)) '(pete roger john keith)))

(who '(sells out))

; 9.5

(define (prepend-every wd sent)
  (every (lambda (item) (word wd item)) sent))

(prepend-every 's '(he aid he aid))
(prepend-every 'anti '(dote pasto gone body))

; 9.6

(define (sentence-version f)
  (lambda (sent) (every f sent)))

((sentence-version first) '(if i fell))
((sentence-version (lambda (x) (* x x))) '(8 2 4 6))

; 9.7

(define (letterwords lett sent)
  (keep (lambda (wd) (member? lett wd)) sent))

(letterwords 'o '(got to get you into my life))

; 9.8

(define (hang secret guessed)
  (accumulate word (every (lambda (l) (if (not (member? l guessed)) '_ l)) secret)))

(hang 'potsticker 'etaoi)

; 9.9

(define (common-words s1 s2)
  (keep (lambda (wd) (member? wd s2)) s1))

(common-words '(a b c d) '(c d e f g))
(common-words '(a b c d e f) '(c))
(common-words '(a b) '(b c d e f g h i j))
(common-words '(a b c d e f) '(a b c d e))

; 9.10

(define (appearances term subj)
  (count (keep (lambda (x) (eq? x term)) subj)))

(appearances 'a 'aabcd)
(appearances 'a '(a b c d e a a d))
(appearances 'x '(a b c d e a a d))


; 9.11


(define (unabbrev s1 s2)
  (every (lambda (wd) (if (number? wd) (item wd s2) wd)) s1))

(unabbrev '(john 1 wayne fred 4) '(bill hank kermit joey))
(unabbrev '(i 3 4 tell 2) '(do you want to know a secret?))

; 9.12

(define (first-last sent)
  (keep (lambda (wd) (eq? (first wd) (last wd))) sent))

(first-last '(california ohio nebraska alabama alaska massachusetts))

; 9.13

(define (compose f g)
  (lambda (x) (f (g x))))

((compose sqrt abs) -25)
(second '(higher order function))

; 9.14

(define (substitute wd1 wd2 sent)
  (every (lambda (wd) (if (eq? wd wd1) wd2 wd)) sent))

(substitute 'maybe 'yeah '(she loves you yeah yeah yeah))

; 9.15

(define (type-check f pred)
  (lambda (x) (if (pred x) (f x) #f)))

(define safe-sqrt (type-check sqrt number?))
(safe-sqrt 16)
(safe-sqrt 'sarsaparilla)

; 9.16

(define (aplize f)
  (lambda (x)
    (cond ((number? x) (f x))
          ((sentence? x) (every f x))
          (else #f))))
(define apl-sqrt (aplize sqrt))

(apl-sqrt 36)
(apl-sqrt '(1 100 25 16))

; 9.17

(define (keep2 pred seq)
  (accumulate se (every (lambda (e) (if (pred e) e '())) seq)))

(keep2 even? '(2 3 4 5 6 7 8))
(keep2 (lambda (l) (member? l 'aeiou)) '(a b c d e f g h i))
(keep2 (lambda (l) (member? l 'aeiou)) 'abcdefghij)