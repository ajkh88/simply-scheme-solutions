#lang simply-scheme

(define (card-val card)
  (let ((val (bf card)))
     (cond ((eq? val 'a) 4)
           ((eq? val 'k) 3)
           ((eq? val 'q) 2)
           ((eq? val 'j) 1)
           (else 0))))

(define (high-card-points hand)
  (accumulate + (every card-val hand)))

(define (count-suit suit hand)
  (count (keep (lambda (card) (eq? (first card) suit)) hand)))

(define (suit-counts hand)
  (every (lambda (suit) (count-suit suit hand)) 'shcd))

(define (suit-dist-points cards-in-suit)
  (cond ((eq? cards-in-suit 2) 1)
        ((eq? cards-in-suit 1) 2)
        ((eq? cards-in-suit 0) 3)
        (else 0)))

(define (hand-dist-points hand)
  (accumulate + (every suit-dist-points (suit-counts hand))))

(define (bridge-val hand)
  (+ (hand-dist-points hand) (high-card-points hand)))


(bridge-val '(sa s10 s7 s6 s2 hq hj h9 ck c4 dk d9 d3))
(bridge-val '(h3 d7 sk s3 c10 dq d8 s9 s4 d10 c7 d4 s2))
