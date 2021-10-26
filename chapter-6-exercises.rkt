#lang simply-scheme

#|

Boring Exercises

6.1

(cond ((= 3 4) '(this boy))
      ((< 2 5) '(nowhere man))
      (else '(two of us)))

Returns: '(nowhere man)

(cond (empty? 3)
      (square 7)
      (else 9))

Returns: 9

(define (third-person-singular verb)
  (cond ((equal? verb 'be) 'is)
        ((equal? (last verb) 'o) (word verb 'es))
        (else (word verb 's))))

(third-person-singular 'go)

Returns: goes

6.2

(or #f #f #f #t)

#t

(and #f #f #f #t)

#f

(or (= 2 3) (= 4 3))

#f

(not #f)

#t

(or (not (= 2 3)) (= 4 3))

#t

(or (and (= 2 3) (= 3 3)) (and (< 2 3) (< 3 4)))

#t

6.3


(define (sign number)
  (if (< number 0)
      'negative
      (if (= number 0)
	  'zero
	  'positive)))

(define (sign number)
  (cond ((< number 0) 'negative)
        ((= number 0) 'zero)
         (else 'positive))))

6.4

(define (utensil meal)
  (cond ((equal? meal 'chinese) 'chopsticks)
	(else 'fork)))

(define (utensil meal)
  (if (equal? meal 'chinese)
       'chopsticks
       'fork))

|#

; Real Exercises

; 6.5

(define (european-time time)
  (cond ((equal? '(12 pm) time) 12)
        ((equal? '(12 am) time) 24)
        ((member? 'pm time) (+ 12 (first time)))
        (else (first time))))
  

(european-time '(8 am))
(european-time '(4 pm))
(european-time '(12 pm))


(define (american-time time)
  (cond ((equal? 12 time) '(12 pm))
        ((equal? 0 time) '(12 am))
        ((> time 12) (se (- time 12) 'pm))
        (else (se time 'am))))

(american-time 21)
(american-time 12)
(european-time '(12 am))

; 6.6

(define (teen? age)
  (and (>= age 13) (<= age 19)))

(teen? 13)
(teen? 14)
(teen? 19)
(teen? 8)
(teen? 20)

; 6.7

(define (type-of val)
  (cond ((sentence? val) 'sentence)
        ((number? val) 'number)
        ((word? val) 'word)
        ((boolean? val) 'boolean)
        (else 'unknown)))

(type-of #f)
(type-of 12)
(type-of '(a b c))
(type-of 'abc)
(type-of +)


; 6.8

(define (vowel? letter)
  (member? letter 'aeiou))


(define (indef-article wd)
  (if (vowel? (first wd))
      (se 'an wd)
      (se 'a wd)))

(indef-article 'beatle)
(indef-article 'album)

; 6.9

(define (plural wd)
  (cond ((and (equal? (last wd) 'y) (vowel? (last (bl wd)))) (word wd 's))
        ((equal? (last wd) 'y) (word (bl wd) 'ies))
        ((member? (last wd) '(x s ss sh ch z es o)) (word wd 'es))
        (else (word wd 's))))

(define (thismany num item)
  (if (= 1 num)
      (se num item)
      (se num (plural item))))

(thismany 1 'partridge)
(thismany 3 'french-hen)

; 6.10

(define (sort2 sent)
  (if (> (first sent) (last sent))
      (se (last sent) (first sent))
      sent))

(sort2 '(5 7))
(sort2 '(7 5))
(sort2 '(7 7))


; 6.11

(define (divisible? big small)
  (= (remainder big small) 0))

(define (leap-year? year)
  (cond ((and (divisible? year 100) (not (divisible? year 400))) #f)
        (else (divisible? year 4))))


(define (num-days-in-month month year)
  (cond ((member? month '(1 3 5 7 8 10 12)) 31)
        ((member? month '(4 6 9 11)) 30)
        ((eq? 2 month)
         (if (leap-year? year) 29 28))))


            
(define (valid-date? day month year)
  (cond ((< year 0) #f)
        ((or (< month 1) (> month 12)) #f)
        ((or (< day 1) (> day (num-days-in-month month year))) #f)
        (else #t)))

(valid-date? 4 10 1949)
(valid-date? 4 20 1776)
(valid-date? 0 5 1992)
(valid-date? 29 2 1900)
(valid-date? 29 2 2000)

; 6.12

(define (plural2 wd)
  (cond ((and (equal? (last wd) 'y) (vowel? (last (bl wd)))) (word wd 's))
        ((equal? (last wd) 'y) (word (bl wd) 'ies))
        ((member? (last wd) '(x s z o)) (word wd 'es))
        ((member? (word (last (bl wd)) (last wd)) '(ss sh ch es)) (word wd 'es))
        ((equal? (word (last (bl wd)) (last wd)) 'us) (word wd 'es))
        (else (word wd 's))))

(plural2 'box)
(plural2 'boy)
(plural2 'welly)
(plural2 'car)
(plural2 'potato)
(plural2 'mess)
(plural2 'splash)

; 6.13

(define (greet name)
  (cond ((and (eq? 'david (first name)) (eq? 'livingstone (last name))) '(dr livingstone I presume?))
        ((and (eq? (last name) 'jr) (eq? (first name) 'dr)) (se 'hello 'dr (last (bl name))))
        ((eq? (first name) 'dr) (se 'hello 'dr (last name)))
        ((member? (first name) '(king queen)) '(hello your majesty))
        ((member? (first name) '(prince princess)) '(hello your royal highness))
        (else (se 'hello (first name)))))

(greet '(john lennon))
(greet '(dr marie curie))
(greet '(dr martin luther king jr))
(greet '(queen elizabeth))
(greet '(david livingstone))


; 6.14

(define (describe-time secs)
  (cond ((< secs 60) (se secs 'seconds))
        ((< secs (* 60 60)) (se (/ secs 60) 'minutes))
        ((< secs (* 60 60 24)) (se (/ secs (* 60 60)) 'hours))
        ((< secs (* 60 60 24 365)) (se (/ secs (* 60 60 24)) 'days))
        ((< secs (* 60 60 24 365 100)) (se (/ secs (* 60 60 24 365)) 'years))
        (else (se (/ secs (* 60 60 24 365 100)) 'centuries))))

(describe-time 45)
(describe-time 930)
(describe-time 9930)
(describe-time 9930)
(describe-time 86400)
(describe-time (* 2 86400))
(describe-time (* 2  (* 365 60 60 24)))
(describe-time 30000000000)
  