#lang simply-scheme

(define (ttt position me)
  (ttt-choose (find-triples position) me))

(define (ttt-choose triples me)
  (cond ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples)) ))

; I can win?
(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position)))

(define (substitute-triple combination position)
  (accumulate word
	      (every (lambda (square)
		       (substitute-letter square position))
		     combination)))

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
         '(123 456 789 147 258 369 159 357)))


(define (appearances term subj)
  (count (keep (lambda (x) (eq? x term)) subj)))

(define (opponent letter)
  (if (equal? letter 'x) 'o 'x))

(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

(define (i-can-win? triples me)
  (choose-win
   (keep (lambda (triple) (my-pair? triple me))
	 triples)))

(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples))))

; Opponent can win?

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)))

; I can fork?
(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me))
			  triples)))

(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
         (keep (lambda (wd) (>= (count wd) 2))
	       (sort-digits (accumulate word sent)))))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
	 '(1 2 3 4 5 6 7 8 9)))

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)))

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent)))

; I can advance?

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
             triples
             me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me)))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
		      (keep number? my-triple)))

(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

; Best free square

(define (best-free-square triples)
  (first-choice (accumulate word triples)
                '(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
               preferences)))



; Exercises


; 10.1
; Already won?

(define (already-won? position player)
  (member? (word player player player) (find-triples position)))

(already-won? '_xo_x_ox_ 'x)
(already-won? '_xo_x_ox_ 'o)

; 10.2
#|
(define (tie-game? position)
  (not (member? '_ position)))
|#


; 10.3

(define (tie-game? position)
  (let ((triples (find-triples position)))
    (if (<= (appearances '_ position) 2)
        (and (not (i-can-win? triples 'o)) (not (i-can-win? triples 'o)))
        #f)))

(tie-game? 'oxooxxxox)
(tie-game? 'oxooxxxo_)
(tie-game? 'oxoox_xo_)
(tie-game? '_________)

; 10.4

#|

• What if you could win a game by having three squares forming an L shape in a corner, such as squares 1, 2, and 4?
Seems to work fine 
• What if the diagonals didn't win?
Again seems to work
• What if you could win by having four squares in a corner, such as 1, 2, 4, and 5?
Doesn't seem to work - investigate why...

10.5

Hahahahahahahaahah

|#


