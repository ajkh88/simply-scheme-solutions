#lang simply-scheme

#|
Definitley needs refinement - I think I made far too many functions with really specific uses.
Could do with a work over, and more generic options chosen. I will get back to it some point.
|#

; Sorting

; ah = aces high
; al = aces low

(define (rank-order-ah card)
  (- 14 (count (member (bf card) '(2 3 4 5 6 7 8 9 10 j q k a)))))

(define (rank-order-al card)
  (- 14 (count (member (bf card) '(a 2 3 4 5 6 7 8 9 10 j q k)))))

(define (suit-order card)
  (- 5 (count (member (first card) '(c d h s)))))

(define (rank-after-al? c1 c2)
  (> (rank-order-al c1) (rank-order-al c2)))

(define (rank-after-ah? c1 c2)
  (> (rank-order-ah c1) (rank-order-ah c2)))

(define (suit-before? c1 c2)
  (< (suit-order c1) (suit-order c2)))

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (first sent) (one-half (bf (bf sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent)) (other-half (bf (bf sent))))))

(define (sort-hand hand merge-on)
  (if (<= (count hand) 1)
      hand
      (merge (sort-hand (one-half hand) merge-on)
             (sort-hand (other-half hand) merge-on) merge-on)))

(define (merge left right merge-on)
  (cond ((empty? left) right)
        ((empty? right) left)
        ((merge-on (first left) (first right))
         (se (first left) (merge (bf left) right merge-on)))
        (else (se (first right) (merge left (bf right) merge-on)))))

(define (sort-rank-ah hand)
  (sort-hand hand rank-after-ah?))

(define (sort-rank-al hand)
  (sort-hand hand rank-after-al?))

(define (sort-suit hand)
  (sort-hand hand suit-before?))

; Utility functions

(define (word-to-num wd)
  (cond ((eq? wd 'one) 1)
        ((eq? wd 'two) 2)
        ((eq? wd 'three) 3)
        ((eq? wd 'four) 4)))

(define (num-to-word n)
  (cond ((eq? n 1) 'one)
        ((eq? n 2) 'two)
        ((eq? n 3) 'three)
        ((eq? n 4) 'four)
        ((eq? n 5) 'five)))

(define (incr-word wd)
  (num-to-word (+ 1 (word-to-num wd))))

(define (get-rank card)
  (bf card))

(define (get-suit card)
  (first card))

(define (appearances term subj)
  (count (keep (lambda (x) (eq? x term)) subj)))

(define (get-next-rank r)
  (cond ((eq? r 'k) 'a)
        ((eq? r 'q) 'k)
        ((eq? r 'j) 'q)
        ((eq? r 10) 'j)
        ((eq? r 'a) '2)
        (else (+ 1 r))))

(define (get-suit-name s)
  (cond ((eq? s 's) 'spades)
        ((eq? s 'c) 'clubs)
        ((eq? s 'd) 'diamonds)
        ((eq? s 'h) 'hearts)))

(define (get-rank-name r)
  (cond ((eq? r 'a) 'ace)
        ((eq? r '2) 'two)
        ((eq? r '3) 'three)
        ((eq? r '4) 'four)
        ((eq? r '5) 'five)
        ((eq? r '6) 'six)
        ((eq? r '7) 'seven)
        ((eq? r '8) 'eight)
        ((eq? r '9) 'nine)
        ((eq? r '10) 'ten)
        ((eq? r 'j) 'jack)
        ((eq? r 'q) 'queen)
        ((eq? r 'k) 'king)))

(define (vowel? l) (member? l 'aeiou))
        
(define (plural wd)
  (cond ((and (equal? (last wd) 'y) (vowel? (last (bl wd)))) (word wd 's))
        ((equal? (last wd) 'y) (word (bl wd) 'ies))
        ((eq? wd 'two) (word wd 's))
        ((member? (last wd) '(x s z o)) (word wd 'es))
        ((member? (word (last (bl wd)) (last wd)) '(ss sh ch es)) (word wd 'es))
        ((equal? (word (last (bl wd)) (last wd)) 'us) (word wd 'es))
        (else (word wd 's))))

; Data structures

(define (compute-ranks-helper hand res)
  (cond ((empty? hand) res)
        ((empty? res) (compute-ranks-helper (bf hand) (se 'one (get-rank (first hand)))))
        ((eq? (last res) (get-rank (first hand)))
         (compute-ranks-helper (bf hand) (se (bl (bl res)) (incr-word (last (bl res))) (last res))))
        (else (compute-ranks-helper (bf hand) (se res 'one (get-rank (first hand)))))))

(define (compute-ranks hand)
  (compute-ranks-helper (sort-rank-ah hand) '()))

(define (index-of term list)
  (cond ((empty? list) #f)
        ((eq? term (first list)) 1)
        (else (+ 1 (index-of term (bf list))))))
  
(define (indices-of-helper term list res idx)
  (cond ((empty? list) res)
        ((eq? term (first list)) (indices-of-helper term (bf list) (se res idx) (+ 1 idx)))
        (else (indices-of-helper term (bf list) res (+ 1 idx)))))

(define (indices-of-twos list)
  (indices-of-helper 'two list '() 1))

(define (get-amounts-pairs computed)
  (se (item (+ 1 (first (indices-of-twos computed))) computed) (item (+ 1 (last (indices-of-twos computed))) computed)))

(define (get-amount amount computed)
  (item (+ 1 (index-of amount computed)) computed))

(define (get-three hand)
  (get-amount 'three (compute-ranks hand)))

(define (get-four hand)
  (get-amount 'four (compute-ranks hand)))

(define (get-pair hand)
  (get-amount 'two (compute-ranks hand)))

(define (get-pairs hand)
  (get-amounts-pairs (compute-ranks hand)))

(define (compute-suits-helper hand res)
  (cond ((empty? hand) res)
        ((empty? res) (compute-suits-helper (bf hand) (se 'one (get-suit (first hand)))))
         ((eq? (last res) (get-suit (first hand)))
         (compute-suits-helper (bf hand) (se (bl (bl res)) (incr-word (last (bl res))) (last res))))
         (else (compute-suits-helper (bf hand) (se res 'one (get-suit (first hand)))))))

(define (compute-suits hand)
  (compute-suits-helper (sort-suit hand) '()))


; Hand predicates

(define (royal-flush? hand)
  (if (royal-flush-pred hand)
      (get-royal-flush-result hand)
      #f))

(define (get-royal-flush-result hand)
  (se 'royal 'flush '- (get-suit-name (get-suit (first hand)))))

(define (royal-flush-pred hand)
  (if (eq? (get-rank (last (sort-rank-ah hand))) 10)
      (flush-helper (sort-rank-ah hand))
      #f))

(define (flush-helper hand)
    (cond ((= (count hand) 1) #t)
          ((and (eq? (get-suit (first hand)) (get-suit (first (bf hand))))
                (eq? (get-rank (first hand)) (get-next-rank (get-rank (first (bf hand)))))
           (flush-helper (bf hand))))
          (else #f)))

(define (straight-flush? hand)
  (if (straight-flush-pred hand)
      (straight-flush-result hand)
      #f))

(define (straight-flush-result hand)
  (se (word (get-rank-name (get-rank (first (sort-rank-ah hand)))) '- 'high) 'straight 'flush))

(define (straight-flush-pred hand)
  (or (flush-helper (sort-rank-ah hand)) (flush-helper (sort-rank-al hand))))

(define (four-of-a-kind? hand)
  (if (four-of-a-kind-pred hand)
      (four-of-a-kind-result hand)
      #f))

(define (four-of-a-kind-result hand)
  (se 'four 'of 'a 'kind '- 'four (plural (get-rank-name (get-four hand)))))

(define (four-of-a-kind-pred hand)
  (member? 'four (compute-ranks hand)))

(define (full-house? hand)
  (if (full-house-pred hand)
      (full-house-result hand)
      #f))

(define (full-house-result hand)
  (let ((first (first (bf (compute-ranks hand))))
        (second (last (compute-ranks hand))))
    (se 'full-house '- (plural (get-rank-name first)) 'over (plural (get-rank-name second)))))
  
(define (full-house-pred hand)
   (and (member? 'three (compute-ranks hand))  (member? 'two (compute-ranks hand))))

(define (flush? hand)
  (if (flush-pred hand)
      (flush-result hand)
      #f))

(define (flush-result hand)
  (se 'flush '- (get-suit-name (get-suit (first hand)))))

(define (flush-pred hand)
  (member? 'five (compute-suits hand)))

(define (straight? hand)
  (if (straight-pred hand)
      (straight-result hand)
      #f))

(define (straight-pred hand)
   (or (straight-helper (sort-rank-ah hand)) (straight-helper (sort-rank-al hand))))

(define (straight-result hand)
  (se (word (get-rank-name (get-rank (first (sort-rank-ah hand)))) '- 'high) 'straight))

(define (straight-helper hand)
   (cond ((= (count hand) 1) #t)
         ((eq? (get-rank (first hand)) (get-next-rank (get-rank (first (bf hand)))))
          (straight-helper (bf hand)))
         (else #f)))

(define (three-of-a-kind? hand)
  (if (three-of-a-kind-pred hand)
      (three-of-a-kind-result hand)
      #f))

(define (three-of-a-kind-result hand)
  (se 'three 'of 'a 'kind '- 'three (plural (get-rank-name (get-three hand)))))

(define (three-of-a-kind-pred hand)
  (member? 'three (compute-ranks hand)))

(define (two-pair? hand)
  (if (two-pair-pred hand)
      (two-pair-result hand)
      #f))

(define (two-pair-result hand)
  (se 'pair 'of (plural (get-rank-name (first (get-pairs hand)))) 'and 'pair 'of (plural (get-rank-name (last (get-pairs hand))))))

(define (two-pair-pred hand)
  (= (appearances 'two (compute-ranks hand)) 2))

(define (pair? hand)
  (if (pair-pred hand)
      (pair-result hand)
      #f))

(define (pair-result hand)
  (se 'pair 'of (plural (get-rank-name (get-pair hand)))))

(define (pair-pred hand)
  (= (appearances 'two (compute-ranks hand)) 1))

(define (poker-value hand)
  (cond ((royal-flush? hand))
        ((straight-flush? hand))
        ((four-of-a-kind? hand))
        ((full-house? hand))
        ((flush? hand))
        ((straight? hand))
        ((three-of-a-kind? hand))
        ((two-pair? hand))
        ((pair? hand))
        (else '(nothing))))

(poker-value '(dk dq dj d10 da))
(poker-value '(da d2 d3 d4 d5))
(poker-value '(d5 d7 d6 d4 d3))
(poker-value '(d3 h3 s3 c2 d2))
(poker-value '(d3 h3 s3 c3 d10))
(poker-value '(s4 s2 s9 sk s5))
(poker-value '(ha d2 s3 h4 c5))
(poker-value '(h6 d2 s3 h4 c5))
(poker-value '(ha h4 c4 s4 d2))
(poker-value '(d3 c3 d6 c6 h7))
(poker-value '(h6 d6 c9 s2 d10))
(poker-value '(hk s2 c5 d9 sa))
