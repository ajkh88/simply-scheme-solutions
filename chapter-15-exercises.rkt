#lang simply-scheme

; 15.1

(define (log2 n) (/ (log n) (log 2)))

(define (nearest-pos n)
  (round (expt 2 (floor (log2 n)))))


(define (to-binary-helper n pos)
  (cond ((< pos 1) "")
        ((> pos n) (word 0 (to-binary-helper n (/ pos 2))))
        (else (word 1 (to-binary-helper (- n pos) (/ pos 2))))))

(define (to-binary n)
  (to-binary-helper n (nearest-pos n)))

(to-binary 23)

; 15.2

(define (sent-to-word sent)
  (if (empty? sent)
      ""
      (word (first sent) (sent-to-word (bf sent)))))

(define (palindrome-helper wd)
  (cond ((= 1 (count wd)) #t)
        ((and (= 2 (count wd)) (eq? (first wd) (last wd))) #t)
        ((eq? (first wd) (last wd)) (palindrome-helper (bf (bl wd))))
        (else #f)))

(define (palindrome? sent)
  (palindrome-helper (sent-to-word sent)))
#|
(palindrome? '(flee to me remote elf))
(palindrome? '(flee to me remote control))
(palindrome? '(do geese see god))
(palindrome? '(mr owl ate my metal worm))
(palindrome? '(not a palindrome))
(palindrome? '(mr owl ate ny metal worm))
|#

; 15.3

(define (substrings-helper wd)
  (if (empty? wd)
      '()
      (se wd (substrings-helper (bl wd)))))

(define (substrings wd)
  (if (empty? wd)
      '()
      (se (substrings-helper wd) (substrings (bf wd)))))

; (substrings 'alexander)

; 15.4

; 2 funcs 1 to find sub and one to run -

(define (substring-test sub str)
  (cond ((empty? sub) #t)
        ((eq? (first sub) (first str)) (substring-test (bf sub) (bf str)))
        (else #f)))

(define (substring? sub str)
  (cond ((empty? str) #f)
        ((substring-test sub str) #t)
        (else (substring? sub (bf str)))))

(substring? 'ssip 'mississippi)
(substring? 'misip 'mississippi)

; 15.5
(define (get-letters n)
  (cond ((eq? n 2) '(a b c))
        ((eq? n 3) '(d e f))
        ((eq? n 4) '(g h i))
        ((eq? n 5) '(j k l))
        ((eq? n 6) '(m n o))
        ((eq? n 7) '(p r s))
        ((eq? n 8) '(t u v))
        ((eq? n 9) '(w x y))
        (else '(q z *))))

(define (merge-sent wd sent)
  (cond ((empty? sent) '())
        (else (se (word (first sent) wd) (merge-sent wd (bf sent))))))

(define (merge-sents new sent)
  (cond ((empty? new) '())
        ((empty? sent) new)
        (else (se (merge-sent (first new) sent) (merge-sents (bf new) sent)))))

(define (phone-spell-helper n res)
  (cond ((empty? n) res)
        (else (phone-spell-helper (bf n) (merge-sents (get-letters (first n)) res)))))

(define (phone-spell n)
  (phone-spell-helper n '()))
  
(count (phone-spell 2235766))

; 15.6

(define (unscramble-helper sent)
  (cond ((eq? (count sent) 2) sent)
        (else (se (unscramble-helper (bf (bf (bl sent)))) 'that (last sent) (first sent) (first (bf sent))))))

(define (unscramble sent)
  (se '(this is) (unscramble-helper (bf (bf sent)))))

(unscramble '(this is the roach the gladiator killed))
(unscramble '(this is the rat the cat the dog the boy the girl saw owned chased bit))
