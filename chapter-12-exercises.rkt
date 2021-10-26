#lang simply-scheme
(define (reverse wd)
  (if (= (count wd) 1)
      wd
      (word (last wd)
            (reverse (bl wd)))))

(reverse 'bealtes)

(define (evens sent)                         
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent))
	  (evens (bf (bf sent))))))

; Boring Exercises

; 12.1

(define (addup nums)
  (if (empty? nums)
      0
      (+ (first nums) (addup (bf nums)))))

(addup '(1 2 3 4))

; 12.2

(define (acronym sent)
  (if (empty? sent)
      ""
      (word (first (first sent))
	    (acronym (bf sent)))))

(acronym '(every good boy deserves football))

; 12.3

(define (fact-1 num)
  (if (= num -1)
      1
      (* num (fact-1 (- num 1)))))

(fact-1 3)

#|
No we cannot because if the base case is -1 then the function will always multiply by 0
and this will return 0 no matter what argument is passed
|#

; 12.4

(define (f sent)
  (if (empty? sent)
      sent
      (se (f (bf sent)) (first sent))))

(f '(a b c d e f g h))


; Real Exercises

; 12.5

(define (exaggerate-helper wd)
  (cond ((eq? wd 'good) 'great)
        ((eq? wd 'bad) 'terrible)
        ((number? wd) (* 2 wd))
        (else wd)))

(define (exaggerate sent)
  (if (empty? sent)
      '()
      (se (exaggerate-helper (first sent)) (exaggerate (bf sent)))))

(exaggerate '(i ate 3 potstickers))
(exaggerate '(the chow fun is good here))

; 12.6

(define (base-grade grade)
  (cond ((eq? (first grade) 'a) 4.0)
        ((eq? (first grade) 'b) 3.0)
        ((eq? (first grade) 'c) 2.0)
        ((eq? (first grade) 'd) 1.0)
        (else 0)))

(define (grade-modifier grade)
  (cond ((eq? (last grade) '+) 0.33)
        ((eq? (last grade) '-) -0.33)
        (else 0)))

(define (total-marks grades)
  (let ((num (count grades)))
  (if (empty? grades)
      0
      (+ (+ (base-grade (first grades)) (grade-modifier (first grades))) (total-marks (bf grades))))))

(define (gpa grades)
  (/ (total-marks grades) (count grades)))

; 12.7

(define (spell-digit digit)
  (item (+ 1 digit)
	'(zero one two three four five six seven eight nine)))

(define (spell-number num)
  (if (empty? num)
      '()
      (se (spell-digit (first num)) (spell-number (bf num)))))

(spell-number 1971)

; 12.8

(define (numbers sent)
  (cond ((empty? sent) '())
        ((number? (first sent)) (se (first sent) (numbers (bf sent))))
        (else (numbers (bf sent)))))

(numbers '(76 trombones and 110 cornets))

; 12.9

(define (real-word? wd)
  (not (member? wd '(a the an in of and for to with))))

(define (real-words sent)
  (cond ((empty? sent) '())
        ((real-word? (first sent)) (se (first sent) (real-words (bf  sent))))
        (else (real-words (bf sent)))))

(real-words '(to be or not to be))

(real-words '(there is a man on the hill with an orange in a bag of chips and he sings for someone to come with him))

; 12.10

(define (remove wd sent)
  (cond ((empty? sent) '())
        ((eq? (first sent) wd) (remove wd (bf sent)))
        (else (se (first sent) (remove wd (bf sent))))))

(remove 'the '(the song love of the loved by the beatles))

; 12.11

(define (count-rec coll)
  (if (empty? coll)
      0
      (+ 1 (count-rec (bf coll)))))

(count-rec '(test this sentence))
(count-rec 'testthisword)

; 12.12

(define (roman-value rom)
  (cond ((eq? rom 'i) 1)
        ((eq? rom 'v) 5)
        ((eq? rom 'x) 10)
        ((eq? rom 'l) 50)
        ((eq? rom 'c) 100)
        ((eq? rom 'd) 500)
        ((eq? rom 'm) 1000)
        (else 0)))


(define (first-less-than-next? rom)
  (cond ((< (count rom) 2) #f)
        ((< (roman-value (first rom)) (roman-value (first (bf rom)))) #t)
        (else #f)))

(define (add-or-sub rom)
  (if (first-less-than-next? rom)
      -
      +))

(define (arabic rom)
  (let ((operation (add-or-sub rom)))
    (if (empty? rom)
        0
        (operation (arabic (bf rom)) (roman-value (first rom))))))

(arabic 'xiv)
(arabic 'mcmlxxi)
(arabic 'mlxvi)
(arabic 'mcmlxxxviii)

; 12.13
#|
(define (describe-time secs)
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs (* 60 60)) (se (/ secs 60) 'minutes))
        ((< secs (* 60 60 24)) (se (/ secs (* 60 60)) 'hours))
        ((< secs (* 60 60 24 365)) (se (/ secs (* 60 60 24)) 'days))
        ((< secs (* 60 60 24 365 100)) (se (/ secs (* 60 60 24 365)) 'years))
        (else (se (/ secs (* 60 60 24 365 100)) 'centuries))))|#
(define (vowel? l) (member? l 'aeiou))
(define (plural wd)
  (cond ((and (equal? (last wd) 'y) (vowel? (last (bl wd)))) (word wd 's))
        ((equal? (last wd) 'y) (word (bl wd) 'ies))
        ((member? (last wd) '(x s z o)) (word wd 'es))
        ((member? (word (last (bl wd)) (last wd)) '(ss sh ch es)) (word wd 'es))
        ((equal? (word (last (bl wd)) (last wd)) 'us) (word wd 'es))
        (else (word wd 's))))

(define (time-in-secs time)
  (cond ((eq? time 'second) 1)
        ((eq? time 'minute) (* (time-in-secs 'second) 60))
        ((eq? time 'hour) (* (time-in-secs 'minute) 60))
        ((eq? time 'day) (* (time-in-secs 'hour) 24))
        ((eq? time 'year) (* (time-in-secs 'day) 365))
        ((eq? time 'century) (* (time-in-secs 'year) 100))
        (else #f)))
(define (time-string secs unit)
  (if (> (floor (/ secs (time-in-secs unit))) 1)
      (se (floor (/ secs (time-in-secs unit))) (plural unit))
      (se (floor (/ secs (time-in-secs unit)))  unit)))


(define (describe-time secs)
  (cond ((= secs 0) '())
        ((< secs (time-in-secs 'minute)) (se secs 'seconds))
        ((< secs (time-in-secs 'hour)) (se (time-string secs 'minute) (describe-time (remainder secs (time-in-secs 'minute)))))
        ((< secs (time-in-secs 'day)) (se (time-string secs 'hour) (describe-time (remainder secs (time-in-secs 'hour)))))
        ((< secs (time-in-secs 'year)) (se (time-string secs 'day) (describe-time (remainder secs (time-in-secs 'day)))))
        ((< secs (time-in-secs 'century)) (se (time-string secs 'year) (describe-time (remainder secs (time-in-secs 'year)))))
        (else (se (time-string secs 'century) (describe-time (remainder secs (time-in-secs 'century)))))))
(describe-time (+ (* 60 60 24 365 100) 435464))
(describe-time (+ (* 60 60 24 365) (* 60 60 24) (* 60 60) 60))
   