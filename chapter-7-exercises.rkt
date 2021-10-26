#lang simply-scheme

; 7.1

(define (vowel? letter)
  (member? letter 'aeiou))

(define (gertrude wd)
  (let ((indef-article (if (vowel? (first wd)) 'an 'a)))
    (se indef-article wd 'is indef-article wd 'is indef-article wd))) 

(gertrude 'rose)
(gertrude 'iguana)

; 7.2

 (let ((pi 3.14159)
       (pie '(lemon meringue)))
    (se '(pi is) pi '(but pie is) pie))

; 7.3

(define (superlative adjective wd)
  (se (word adjective 'est) wd))

(superlative 'dumb 'exercise)


; 7.4

(define (sum-square a b)
  (let ((+ *)
        (* +))
    (* (+ a a) (+ b b))))
(sum-square 2 2)

#|

This procedure first swaps the value of + and * (in the scope of the function) so that + multiplies and * adds.
The function then adds the values returned by multiplying the arguments by themselves, using the newly defined/redirected functions.

|#

