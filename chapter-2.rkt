#lang simply-scheme

; FUNTIONS

; Arithmtetic
(remainder 5 4)
(sqrt 4)
(expt 5 4)
(quotient 10 2)
(/ 10 2)
(random 10)
(max 20 2)

(word 3.14 1593245)
(count (se 'this 'is 'a 'sent))

(expt -3 -3)
(expt -3 .5)

;(remainder 5 0)

; Sentences

(sentence '(when i get) 'home)
(butfirst 'a)
(butfirst '(yer blues))

; Booleans
(member? 'john '(john gimp))
(equal? (+ 2 2) 4)

; Higher order functions

(define (vowel? letter)
  (member? letter 'aeiou))

(every first '(the long and winding road))
(keep vowel? 'constantinople)
(keep even? 123456789)