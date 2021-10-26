#lang simply-scheme

(define (rank-order-ah card)
  (- 14 (count (member (bf card) '(2 3 4 5 6 7 8 9 10 j q k a)))))

(define (rank-order-al card)
  (- 14 (count (member (bf card) '(a 2 3 4 5 6 7 8 9 10 j q k)))))

(define (rank-before-al? c1 c2)
  (< (rank-order-al c1) (rank-order-al c2)))

(define (rank-before-ah? c1 c2)
  (< (rank-order-ah c1) (rank-order-ah c2)))

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
  (sort-hand hand rank-before-ah?))
(define (sort-rank-al hand)
  (sort-hand hand rank-before-al?))

(sort-rank-ah '(sa c10 c4 d5 d10 hk h2 ha))
(sort-rank-al '(sa c10 c4 d5 d10 hk h2 ha))

(define (word-to-num wd)
  (cond ((eq? wd 'one) 1)
        ((eq? wd 'two) 2)
        ((eq? wd 'three) 3)
        ((eq? wd 'four) 4)))

(define (num-to-word n)
  (cond ((eq? n 1) 'one)
        ((eq? n 2) 'two)
        ((eq? n 3) 'three)
        ((eq? n 4) 'four)))

(define (incr-word wd)
  (num-to-word (+ 1 (word-to-num wd))))

(define (get-rank card)
  (bf card))

(define (get-suit card)
  (first card))

(define (compute-ranks-helper hand res)
  (cond ((empty? hand) res)
        ((not (member? (get-rank (first hand)) res)) (compute-ranks-helper (bf hand) (se res 'one (get-rank (first hand))))) 
        ((eq? (get-rank (last res)) (get-rank (first hand))) (compute-ranks-helper (bf hand) (se (incr-word (bl (bl res))) (get-rank (last res)))))
        (else (compute-ranks-helper (bf hand) res))))


(define (compute-ranks hand)
  (compute-ranks-helper (sort-rank-ah hand) '()))

(compute-ranks '(dq c3 c4 h3 h4))

