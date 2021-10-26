#lang simply-scheme

; 14.1

; keep

(define (remove-once wd sent)
  (cond ((empty? sent) '())
        ((eq? (first sent) wd) (bf sent))
        (else (se (first sent) (remove-once wd (bf sent))))))
        
  

(remove-once 'morning '(good morning good morning))


; 14.2

; accumulate

(define (up wd)
  (if (empty? wd)
      '()
      (se (up (bl wd)) wd)))

(up 'town)


; 14.3

; keep

(define (remdup sent)
  (remdup-helper sent '()))

(define (remdup-helper sent kept)
  (cond ((empty? sent) '())
        ((member? (first sent) kept) (remdup-helper (bf sent) kept))
        (else (se (first sent) (remdup-helper (bf sent) (se (first sent) kept))))))

        
(remdup '(ob la di ob la da))

; 14.4

; keep

(define (odds sent)
  (odds-helper 1 sent))

(define (odds-helper n sent)
  (cond ((empty? sent) '())
        ((odd? n) (se (first sent) (odds-helper (+ n 1) (bf sent))))
        (else (odds-helper (+ n 1) (bf sent)))))

(odds '(i lost my little girl))

; 14.5

(define (letter-count sent)
  (if (empty? sent)
      0
      (+ (count (first sent)) (letter-count (bf sent)))))

(letter-count '(fixing a hole))

; 14.6

(define (member2? item coll)
  (cond ((empty? coll) #f)
        ((eq? (first coll) item) #t)
        (else (member? item (bf coll)))))

(member2? 'a '(a b c d e f))
(member2? 'a '(b c d e f))
(member2? 'a 'abc)
(member2? 'a 'def)

; 14.7

(define (differences sent)
  (cond ((= (count sent) 1) '())
        (else (se (- (first (bf sent)) (first sent)) (differences (bf sent))))))


(differences '(4 23 9 87 6 12))

; 14.8

(define (repeat-word wd n)
  (if (= 0 n)
      '()
      (se wd (repeat-word wd (- n 1)))))

(define (expand sent)
  (cond ((empty? sent) '())
        ((number? (first sent)) (se (repeat-word (first (bf sent)) (first sent)) (expand (bf (bf sent)))))
        (else (se (first sent) (expand (bf sent))))))

(expand '(4 calling birds 3 french hens))

(expand '(the 7 samurai))


; 14.9

(define (location-helper wd sent pos)
  (cond ((empty? sent) #f)
        ((eq? (first sent) wd) pos)
        (else (location-helper wd (bf sent) (+ pos 1)))))

(define (location wd sent)
  (location-helper wd sent 1))

(location 'me '(you never give me your money))
(location 'you '(you never give me your money))
(location 'money '(you never give me your money))

; 14.10

(define (count-adjacent-duplicates-helper sent total)
  (cond ((= (count sent) 1) total)
        ((eq? (first sent) (first (bf sent))) (count-adjacent-duplicates-helper (bf sent) (+ total 1)))
        (else (count-adjacent-duplicates-helper (bf sent) total))))

(define (count-adjacent-duplicates sent)
  (count-adjacent-duplicates-helper sent 0))

(count-adjacent-duplicates '(y a b b a d a b b a d o o))
(count-adjacent-duplicates '(yeah yeah yeah))

; 14.11

(define (remove-adjacent-duplicates sent)
  (cond ((= (count sent) 1) sent)
        ((eq? (first sent) (first (bf sent))) (remove-adjacent-duplicates (bf sent)))
        (else (se (first sent) (remove-adjacent-duplicates (bf sent))))))

(remove-adjacent-duplicates '(y a b b a d a b b a d o o))
(remove-adjacent-duplicates '(yeah yeah yeah))

; 14.12

(define (is-square-of? x y)
  (= (* x x) y))

(define (progressive-squares? sent)
  (cond ((= (count sent) 1) #f)
        ((and (= (count sent) 2) (is-square-of? (first sent) (first (bf sent)))) #t)
        ((is-square-of? (first sent) (first (bf sent))) (progressive-squares? (bf sent)))
        (else #f)))

(progressive-squares? '(3 9 81 6561))

(progressive-squares? '(25 36 49 64))

; 14.13

(define (pigl wd)
  (pigl-helper wd (count wd)))
     

(define (pigl-helper wd len)
  (cond ((= 0 len) (word wd 'ay))
        ((member? (first wd) 'aeiou) (word wd 'ay))
        (else (pigl-helper (word (bf wd) (first wd)) (- len 1)))))
                                    
(pigl 'trumpet)
(pigl 'ant)
(pigl 'throughout)
(pigl 'frzzmlpt)

; 14.14


(define (same-shape? s1 s2)
  (cond ((and (empty? s1) (empty? s2)) #t)
        ((not (= (count s1) (count s2))) #f)
        ((= (count (first s1)) (count (first s2))) (same-shape? (bf s1) (bf s2)))
        (else #f)))

(same-shape? '(the fool on the hill) '(you like me too much))
(same-shape? '(the fool on the hill) '(and your bird can sing))
(same-shape? '(the fool on the hill) '(you like me too much eh))

; 14.15


(define (merge s1 s2)
  (cond ((and (empty? s1) (empty? s2)) '())
        ((empty? s1) s2)
        ((empty? s2) s1)
        ((< (first s1) (first s2)) (se (first s1) (merge (bf s1) s2)))
        (else (se (first s2) (merge s1 (bf s2))))))

(merge '(4 7 18 40 99) '(3 6 9 12 24 36 50))

; 14.16

(define (vowel? l)
  (member? l 'aeiou))
(define (num-of-contigious-vowels wd)
  (cond ((empty? wd) 0)
        ((vowel? (first wd)) (+ 1 (num-of-contigious-vowels (bf wd))))
        (else 0)))

(define (cut-word start wd)
  (if (eq? 0 start)
      wd
      (cut-word (- start 1) (bf wd))))

(cut-word 3 'abcdefghij)


(define (syllables wd)
  (let ((num-vowels (num-of-contigious-vowels wd)))
  (cond ((empty? wd) 0)
        ((> num-vowels 0) (+ 1 (syllables (cut-word num-vowels wd))))
        (else (syllables (bf wd))))))

(syllables 'soaring)
(syllables 'a)
(syllables 'eight)
(syllables 'throughout)
(syllables 'predicament)
(syllables 'it)
(syllables 'zoo)
(syllables 'ciao)
(syllables 'queue)