#lang simply-scheme

; Boring Exercises

; 20.1

; What happens when we evaluate the following expression? What is printed,
; and what is the return value? Try to figure it out in your head before you
; try it on the computer.

(cond ((= 2 3) (show '(lady madonna)) '(i call your name))
      ((< 2 3) (show '(the night before)) '(hello little girl))
      (else '(p.s. i love you)))

#|

'(the night before) and '(hello little girl) will be printed, the first printed to
the console as a side-effect, and the second as the evaluation of the expression
via the REPL

|#

; 20.2

; What does newline return in your version of Scheme?

(newline)

; Nothing? An empty line appears in the console but nothing seems to be returned


; 20.3

; Define show in terms of newline and display.

; (show x) is equivalent to (display x)(newline)

; Real Exercises

; 20.4

; Write a program that carries on a conversation like the following example.
; What the user types is in boldface.

#|
> (converse)
Hello, I'm the computer.  What's your name? Brian Harvey
Hi, Brian.  How are you? I'm fine.
Glad to hear it.

(define (converse)
  (read-line)
  (display "Hello, I'm the computer.  What's your name? ")
  (let ((name (read-line)))
    (display "Hi ")
    (display (first name))
    (display ".  How are you? ")
    (let ((response (read-line)))
      (display "Glad to hear it."))))

(converse)

|#

; 20.5

; Our name-table procedure uses a fixed width for the column containing
; the last names of the people in the argument list.
; Suppose that instead of liking British-invasion music you are into
; late romantic Russian composers:

; (name-table '((piotr tchaikovsky) (nicolay rimsky-korsakov)
;                                   (sergei rachmaninov) (modest musorgsky)))

; Alternatively, perhaps you like jazz:

; (name-table '((bill evans) (paul motian) (scott lefaro)))

; Modify name-table so that it figures out the longest last name in its argument
; list, adds two for spaces, and uses that number as the width of the first column.


(define (ll-helper names longest)
  (cond ((empty? names) longest)
        ((> (count (last (first names))) longest)
         (ll-helper (bf names) (count (last (first names)))))
        (else (ll-helper (bf names) longest))))

(define (longest-last names)
  (ll-helper names (count (last (first names)))))


(define (name-table-helper names longest)
  (if (null? names)
      'done
      (begin (display (align (cadar names) (+ longest 2)))
             (show (caar names))
             (name-table-helper (cdr names) longest))))

(define (name-table names)
  (let ((longest (longest-last names)))
    (name-table-helper names longest)))

        
  
(name-table '((piotr tchaikovsky) (nicolay rimsky-korsakov)
                                  (sergei rachmaninov) (modest musorgsky)))

(name-table '((bill evans) (paul motian) (scott lefaro)))

;=====================================================================

; TIC TAC TOE Functions

(define (ttt position me)
  (ttt-choose (find-triples position) me))

(define (find-triples position)
  (every (lambda (comb) (substitute-triple comb position))
         '(123 456 789 147 258 369 159 357)))

(define (substitute-triple combination position)
  (accumulate word
	      (every (lambda (square)
		       (substitute-letter square position))
		     combination) ))

(define (substitute-letter square position)
  (if (equal? '_ (item square position))
      square
      (item square position) ))

(define (ttt-choose triples me)
  (cond ((i-can-win? triples me))
        ((opponent-can-win? triples me))
        ((i-can-fork? triples me))
        ((i-can-advance? triples me))
        (else (best-free-square triples)) ))

(define (i-can-win? triples me)
  (choose-win
   (keep (lambda (triple) (my-pair? triple me))
         triples)))

(define (my-pair? triple me)
  (and (= (appearances me triple) 2)
       (= (appearances (opponent me) triple) 0)))

(define (opponent letter)
  (if (equal? letter 'x) 'o 'x))

(define (choose-win winning-triples)
  (if (empty? winning-triples)
      #f
      (keep number? (first winning-triples)) ))

(define (opponent-can-win? triples me)
  (i-can-win? triples (opponent me)) )

(define (i-can-fork? triples me)
  (first-if-any (pivots triples me)) )

(define (first-if-any sent)
  (if (empty? sent)
      #f
      (first sent) ))

(define (pivots triples me)
  (repeated-numbers (keep (lambda (triple) (my-single? triple me))
                          triples)))

(define (my-single? triple me)
  (and (= (appearances me triple) 1)
       (= (appearances (opponent me) triple) 0)))

(define (repeated-numbers sent)
  (every first
         (keep (lambda (wd) (>= (count wd) 2))
               (sort-digits (accumulate word sent)) )))

(define (sort-digits number-word)
  (every (lambda (digit) (extract-digit digit number-word))
         '(1 2 3 4 5 6 7 8 9) ))

(define (extract-digit desired-digit wd)
  (keep (lambda (wd-digit) (equal? wd-digit desired-digit)) wd))

(define (i-can-advance? triples me)
  (best-move (keep (lambda (triple) (my-single? triple me)) triples)
             triples
             me))

(define (best-move my-triples all-triples me)
  (if (empty? my-triples)
      #f
      (best-square (first my-triples) all-triples me) ))

(define (best-square my-triple triples me)
  (best-square-helper (pivots triples (opponent me))
		      (keep number? my-triple)))

(define (best-square-helper opponent-pivots pair)
  (if (member? (first pair) opponent-pivots)
      (first pair)
      (last pair)))

(define (best-free-square triples)
  (first-choice (accumulate word triples)
                '(5 1 3 7 9 2 4 6 8)))

(define (first-choice possibilities preferences)
  (first (keep (lambda (square) (member? square possibilities))
               preferences)))

(define (stupid-ttt position letter)
  (location '_ position))

(define (location letter word)
  (if (equal? letter (first word))
      1
      (+ 1 (location letter (bf word)))))
;=====================================================================
; 20.9
; The way we invoke the game program isn't very user-friendly.
; Write a procedure game that asks you whether you wish to play x or o,
; then starts a game. (By definition, x plays first.) Then write a
; procedure games that allows you to keep playing repeatedly.
; It can ask "do you want to play again?" after each game.
; (Make sure that the outcome of each game is still reported,
; and that the user can choose whether to play x or o before each game.)

(define (game message)
  (start-game message))
 
(define (start-game message)
  (show message)
   (let ((player (read)))
    (cond ((or (eq? player 'x) (eq? player 'X)) (play-ttt ask-user ttt))
          ((or (eq? player 'o) (eq? player 'O)) (play-ttt ttt ask-user))
          (else (start-game "Please enter O or X to choose what you would like to play as")))))

(define (play-again)
     (display "Would you like to play again? (Y or N)")
      (newline)
      (let ((answer (read)))
        (cond ((or (eq? answer 'Y) (eq? answer 'y)) #t)
              ((or (eq? answer 'N) (eq? answer 'n)) #f)
              (else (show "Please enter Y or N") (games)))))
  
(define (games message)
  (if (game message)
      (if (play-again)
          (games "Please enter O or X to choose what you would like to play as")
          (show "Thanks for playing"))
      #f))
        

(define (play-ttt x-strat o-strat)
  (play-ttt-helper x-strat o-strat '_________ 'x))

(define (already-won? position who)
  (member? (word who who who)  (find-triples position)))

(define (tie-game? position)
  (not (member? '_ position)))
;=====================================================================
; 20.8
;
; At the end of the game, if the computer wins or ties, you never find
; out which square it chose for its final move. Modify the program to
; correct this. (Notice that this exercise requires you to make play-ttt-helper
; non-functional.)

(define (play-ttt-helper x-strat o-strat position whose-turn)
  (cond ((already-won? position (opponent whose-turn))
         (print-position position)
	 (show (list (opponent whose-turn) 'wins!)) #t)
	((tie-game? position) '(tie game))
	(else (let ((square (if (equal? whose-turn 'x)
				(x-strat position 'x)
				(o-strat position 'o))))
		(play-ttt-helper x-strat
				 o-strat
				 (add-move square whose-turn position)
				 (opponent whose-turn))))))

(define (add-move square letter position)
  (if (= square 1)
      (word letter (bf position))
      (word (first position)
	    (add-move (- square 1) letter (bf position)))))


;=====================================================================
; 20.6
;
; The procedure ask-user isn't robust. What happens if you type something
; that isn't a number, or isn't between 1 and 9? Modify it to check that
; what the user types is a number between 1 and 9. If not, it should print
; a message and ask the user to try again.

; 20.7
;
; Another problem with ask-user is that it allows a user to request a square
; that isn't free. If the user does this, what happens? Fix ask-user to ensure
; that this can't happen.



(define (ask-again position letter message)
  (show message)
   (ask-user position letter))

(define (already-taken position move letter)
  (if (not (eq? (item move position) '_))
      (ask-again position letter "Move already taken - try again...")
      #f))

(define (invalid-move position move letter)
  (if (or (not (number? move)) (< move 1) (> move 9))
        (ask-again position letter "Move must be a number between 1 and 9 - try again...")
        #f))

(define (ask-user position letter)
  (print-position position)
  (display letter)
  (display "'s move: ")
  (let ((move (read)))
    (cond ((invalid-move position move letter))
          ((already-taken position move letter))
          (else move))))
  


(define (print-position position)
  (print-row (subword position 1 3))
  (show "-+-+-")
  (print-row (subword position 4 6))
  (show "-+-+-")
  (print-row (subword position 7 9))
  (newline))

(define (print-row row)
  (maybe-display (first row))
  (display "|")
  (maybe-display (first (bf row)))
  (display "|")
  (maybe-display (last row))
  (newline))

(define (maybe-display letter)
  (if (not (equal? letter '_))
      (display letter)
      (display " ")))

(define (subword wd start end)
  ((repeated bf (- start 1))
   ((repeated bl (- (count wd) end))
    wd)))

(games "Welcome TIC:TAC:TOE - would you like to play as O's or X's?")



