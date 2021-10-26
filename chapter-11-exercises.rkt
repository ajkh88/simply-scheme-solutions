#lang simply-scheme


(define (explode wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))

(explode 'dynamite)

(define (letter-pairs wd)
  (if (<= (count wd) 1)
      '()
      (se (word (first wd) (first (bf wd))) (letter-pairs (bf wd)))))

(letter-pairs 'george)


; Boring Exercises

; 11.1



(define (downup4 wd)
  (se wd (bl wd) (bl (bl wd)) (first wd) (bl (bl wd)) (bl wd) wd))

(downup4 'food)



; 11.2

(define (count-ums sent)
  (cond ((empty? sent) 0)
        ((eq? (first sent) 'um) (+ 1 (count-ums (bf sent))))
        (else (count-ums (bf sent)))))

(count-ums '(today um we are going to um talk about the combining um method))

; 11.3

(define (unspell-letter letter)
  (cond ((member? letter 'abc) 2)
	((member? letter 'def) 3)
	((member? letter 'ghi) 4)
	((member? letter 'jkl) 5)
	((member? letter 'mno) 6)
	((member? letter 'pqrs) 7)
	((member? letter 'tuv) 8)
	((member? letter 'wxyz) 9)
	(else 0)))

(define (phone-unspell wd)
  (if (empty? wd)
      ""
      (word (unspell-letter (first wd)) (phone-unspell (bf wd)))))

(phone-unspell 'popcorn)

; 11.4
; No idea - was it from this book? Is it some sort of recursion joke? :)

; 11.5

(define (initials sent)
  (if (empty? sent)
      '()
      (se (first (first sent)) (initials (bf sent)))))

(initials '(if i needed someone))

; 11.6

(define (countdown num)
  (if (= num 0)
      '(BLASTOFF!)
      (se num (countdown (- num 1)))))

(countdown 10)

; 11.7

(define (copies num wd)
  (if (= num 0)
      '()
      (se wd (copies (- num 1) wd))))

(copies 8 'spam)