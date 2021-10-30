#lang simply-scheme

; Boring Exercises

; 17.1

; What will Scheme print in response to each of the following expressions?
; Try to figure it out in your head before you try it on the computer.

; (car '(Rod Chris Colin Hugh Paul))

; > 'Rod

; (cadr '(Rod Chris Colin Hugh Paul))

; > 'Chris

; (cdr '(Rod Chris Colin Hugh Paul))

; > '(Chris Colin Hugh Paul)

; (car 'Rod)

; > 'R

; Actually retuns error >

#|
car: contract violation
  expected: pair?
  given: 'Rod
|#

; (cons '(Rod Argent) '(Chris White))

; > '((Rod Argent) Chris White)

; (append '(Rod Argent) '(Chris White))

; > '(Rod Argent Chris White)

; (list '(Rod Argent) '(Chris White))

; > '((Rod Argent) (Chris White))

; (caadr '((Rod Argent) (Chris White)
;          (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))

; > 'Chris

; (assoc 'Colin '((Rod Argent) (Chris White)
;		  (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))

; > '(Colin Blunstone) 

; (assoc 'Argent '((Rod Argent) (Chris White)
;	           (Colin Blunstone) (Hugh Grundy) (Paul Atkinson)))

; > #f

; 17.2

; For each of the following examples, write a procedure of two arguments that,
; when applied to the sample arguments, returns the sample result.
; Your procedures may not include any quoted data.

(define (f1 l1 l2)
  (list (append (cdr l1) (cons (car l2) '()))))

(f1 '(a b c) '(d e f))

(define (f2 l1 l2)
  (cons (cdr l1) (cons (cadr l2) '())))

(f2 '(a b c) '(d e f))

(define (f3 l1 l2)
  (append l1 l1))

(f3 '(a b c) '(d e f))

(define (f4 l1 l2)
  ( list (cons (car l1) (cons (car l2) '())) (append (cdr l1) (cdr l2))))

(f4 '(a b c) '(d e f))

; 17.3

; Describe the value returned by this invocation of map:

 (map (lambda (x) (lambda (y) (+ x y))) '(1 2 3 4))
#|
This invocation of map applies the function (x => y => x + y) to the list (1 2 3 4)
This means the list now consists of a list of functions which add the original element to
the passed argument
|#

; Real Exercises

; 17.4

; Describe the result of calling the following procedure with a list as its argument.
; (See if you can figure it out before you try it.)

(define (mystery lst)
  (mystery-helper lst '()))

(define (mystery-helper lst other)
  (if (null? lst)
      other
      (mystery-helper (cdr lst) (cons (car lst) other))))

#|
This will take a list as an argument, and for every element in the list it will
add the element to the start of new list, eventually returning the reversed list
|#

; 17.5

; Here's a procedure that takes two numbers as arguments and
; returns whichever number is larger:

(define (max2 a b)
  (if (> b a) b a))

; Use max2 to implement max, a procedure that takes one or more
; numeric arguments and returns the largest of them.

(define (max num . rest-of-nums)
  (if (null? rest-of-nums)
      num
      (apply max (cons (max2 num (car rest-of-nums)) (cdr rest-of-nums)))))

(max 2 3 8 6 3 5 1 7 9)

; 17.6

; Implement append using car, cdr, and cons.
; (Note: The built-in append can take any number of arguments.
;  First write a version that accepts only two arguments. Then, optionally,
;  try to write a version that takes any number.)


(define (get-last lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) (car lst))
        (else (get-last (cdr lst)))))

(define (get-butlast lst)
  (cond ((null? lst) lst)
        ((null? (cdr lst)) (cdr lst))
        (else (cons (car lst) (get-butlast (cdr lst))))))

(define (new-append l1 l2)
  (cond ((null? l1) l2)
        ((null? l2) l1)
        (else (new-append (get-butlast l1) (cons (get-last l1) l2)))))

(new-append '(a b c) '(d e f))



(define (new-append-var lst . rest)
  (cond ((null? rest) lst)
        ((null? lst) (apply new-append-var (car rest) (cdr rest)))
        (else (apply new-append-var (new-append lst (car rest)) (cdr rest)))))

(new-append-var '() '(d e f) '() '(j k l))

; 17.7

;  Append may remind you of sentence. They're similar, except that append
; works only with lists as arguments, whereas sentence will accept words as
; well as lists. Implement sentence using append.

; (Note: The built-in sentence can take any number of arguments.
;  First write a version that accepts only two arguments. Then,
;  optionally, try to write a version that takes any number.
;  Also, you don't have to worry about the error checking that
;  the real sentence does.)

(define (new-sent e1 e2)
  (cond ((null? e1) e2)
        ((null? e2) e1)
        ((and (word? e1) (word? e2)) (append (cons e1 '()) (cons e2 '())))
        ((word? e1) (append (cons e1 '()) e2))
        ((word? e2) (append e1 (cons e2 '())))
        (else (append e1 e2))))
        

(define (new-sent-var sent . rest)
  (cond ((null? rest) sent)
        ((null? sent) (apply new-sent-var (car rest) (cdr rest)))
        (else (apply new-sent-var (new-sent sent (car rest)) (cdr rest)))))

(new-sent-var 'abc '(d e f) 'ghi '(j k l))

; 17.8

; Write member.

(define (new-member elem lst)
  (cond ((null? lst) #f)
        ((equal? elem (car lst)) #t)
        (else (new-member elem (cdr lst)))))

; 17.9

; Write list-ref.


(define (new-list-ref lst idx)
  (cond ((equal? idx 0) (car lst))
        ((null? lst) #f)
        (else (new-list-ref (cdr lst) (- idx 1)))))

(new-list-ref '(happiness is a warm gun) 4)

; 17.10

; Write length.

(define (new-length lst)
  (if (null? lst)
      0
      (+ 1 (new-length (cdr lst)))))

(new-length '(a b c d e f))


; 17.11

;  Write before-in-list?, which takes a list and two elements of the list.
; It should return #t if the second argument appears in the list argument
; before the third argument:

; > (before-in-list? '(back in the ussr) 'in 'ussr)
; #T

; > (before-in-list? '(back in the ussr) 'the 'back)
; #F

; The procedure should also return #f if either of the supposed elements
; doesn't appear at all.

(define (before-in-list? lst first second)
  (cond ((equal? (car lst) second) #f)
        ((equal? (car lst) first) #t)
        (else (before-in-list? (cdr lst) first second))))

(before-in-list? '(back in the ussr) 'in 'ussr)
(before-in-list? '(back in the ussr) 'the 'back)

; 17.12

(define (flatten lst)
  (cond ((null? lst) '())
        ((word? (car lst)) (cons (car lst) (flatten (cdr lst))))
        (else (append (flatten (car lst)) (flatten (cdr lst))))))

(flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

; 17.13

; Here is a procedure that counts the number of words anywhere within a structured list:

#|
(define (deep-count lst)
  (cond ((null? lst) 0)
	((word? (car lst)) (+ 1 (deep-count (cdr lst))))
	(else (+ (deep-count (car lst))
		 (deep-count (cdr lst))))))
|#

; Although this procedure works, it's more complicated than necessary. Simplify it.


(define (deep-count lst)
   (cond ((null? lst) 0)
         ((word? lst) 1)
         (else (reduce + (map (lambda (sublist) (deep-count sublist)) lst)))))

(deep-count '(1 2 (3 (a b) c) 4 d))

; 17.14

; Write a procedure branch that takes as arguments a list of numbers
; and a nested list structure. It should be the list-of-lists equivalent
; of item, like this:

; > (branch '(3) '((a b) (c d) (e f) (g h)))
; (E F)

; > (branch '(3 2) '((a b) (c d) (e f) (g h)))
; F

; > (branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))
; H

; In the last example above, the second element of the list is

; ((C D) (E F) ((G H) (I J)) K)

; The third element of that smaller list is ((G H) (I J));
; the first element of that is (G H); and the second element of that is just H.


(define (branch path tree)
  (if (null? path)
      tree
      (branch (cdr path) (list-ref tree (- (car path) 1)))))

(branch '(3) '((a b) (c d) (e f) (g h)))
(branch '(3 2) '((a b) (c d) (e f) (g h)))
(branch '(2 3 1 2) '((a b) ((c d) (e f) ((g h) (i j)) k) (l m)))

; 17.15

; Modify the pattern matcher to represent the known-values database as
; a list of two-element lists, as we suggested at the beginning of this chapter. 

; ==============================================================================
; ==============================================================================

; Copy source of match.scm to edit for answer

(define (match pattern sent)
  (match-using-known-values pattern sent '()))

(define (match-using-known-values pattern sent known-values)
  (cond ((empty? pattern)
	 (if (empty? sent) known-values 'failed))
	((special? (first pattern))
	 (let ((placeholder (first pattern)))
	   (match-special (first placeholder)
			  (bf placeholder)
			  (bf pattern)
			  sent
			  known-values)))
	((empty? sent) 'failed)
	((equal? (first pattern) (first sent))
	 (match-using-known-values (bf pattern) (bf sent) known-values))
	(else 'failed)))

(define (special? wd)
  (member? (first wd) '(* & ? !)))

(define (match-special howmany name pattern-rest sent known-values)
  (let ((old-value (lookup name known-values)))
    (cond ((not (equal? old-value 'no-value))
	   (if (length-ok? old-value howmany)
	       (already-known-match
		  old-value pattern-rest sent known-values)
	       'failed))
	  ((equal? howmany '?)
	   (longest-match name pattern-rest sent 0 #t known-values))
	  ((equal? howmany '!)
	   (longest-match name pattern-rest sent 1 #t known-values))
	  ((equal? howmany '*)
	   (longest-match name pattern-rest sent 0 #f known-values))
	  ((equal? howmany '&)
	   (longest-match name pattern-rest sent 1 #f known-values)))))

(define (length-ok? value howmany)
  (cond ((empty? value) (member? howmany '(? *)))
	((not (empty? (bf value))) (member? howmany '(* &)))
	(else #t)))

(define (already-known-match value pattern-rest sent known-values)
  (let ((unmatched (chop-leading-substring value sent)))
    (if (not (equal? unmatched 'failed))
	(match-using-known-values pattern-rest unmatched known-values)
	'failed)))

(define (chop-leading-substring value sent)
  (cond ((empty? value) sent)
	((empty? sent) 'failed)
	((equal? (first value) (first sent))
	 (chop-leading-substring (bf value) (bf sent)))
	(else 'failed)))

(define (longest-match name pattern-rest sent min max-one? known-values)
  (cond ((empty? sent)
	 (if (= min 0)
	     (match-using-known-values pattern-rest
				       sent
				       (add name '() known-values))
	     'failed))
	(max-one?
	 (lm-helper name pattern-rest (se (first sent))
		    (bf sent) min known-values))
	(else (lm-helper name pattern-rest
			 sent '() min known-values))))

(define (lm-helper name pattern-rest
		   sent-matched sent-unmatched min known-values)
  (if (< (length sent-matched) min)
      'failed
      (let ((tentative-result (match-using-known-values
			       pattern-rest
			       sent-unmatched
			       (add name sent-matched known-values))))
	(cond ((not (equal? tentative-result 'failed)) tentative-result)
	      ((empty? sent-matched) 'failed)
	      (else (lm-helper name
			       pattern-rest
			       (bl sent-matched)
			       (se (last sent-matched) sent-unmatched)
			       min
			       known-values))))))

;;; Known values database abstract data type

(define (lookup name known-values)
  (let ((record (assoc name known-values)))
    (if record
        (cadr record)
        'no-value)))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (cons (list name value) known-values)))

; ==============================================================================
; ==============================================================================

(match '(!first b c *last) '(a b c))
(match '(!first b c *last) '(a b c d))


; 17.16

; Write a predicate valid-infix? that takes a list as argument and
; returns #t if and only if the list is a legitimate infix arithmetic
; expression (alternating operands and operators, with parentheses—that is,
; sublists—allowed for grouping).

; > (valid-infix? '(4 + 3 * (5 - 2)))
; #T

; > (valid-infix? '(4 + 3 * (5 2)))
; #F

(define (is-operator? x)
  (member? x '+*/-))

(define (not-op-or-sub x)
  (not (or (is-operator? x) (list? x))))

(define (valid-infix-helper expr)
  (cond ((null? (cdr expr)) #t)
        ((and (number? (car expr)) (number? (cadr expr))) #f)
        ((and (number? (car expr)) (not-op-or-sub (cadr expr))) #f)
        (else (valid-infix-helper (cdr expr)))))

(define (valid-infix? expr)
  (if (number? expr)
      #t
      (valid-infix-helper (flatten expr))))

       
(valid-infix? '(4 + 3 * (5 - 2)))
(valid-infix? '(4 + 3 * (5 2)))
(valid-infix? '(4 d 3 * (5 2)))
