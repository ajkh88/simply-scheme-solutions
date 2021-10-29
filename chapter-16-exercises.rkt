#lang simply-scheme

; ==============================================================================
; ==============================================================================

; Copy source of match.scm - require/load do not work with #lang simply-scheme

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
  (cond ((empty? known-values) 'no-value)
	((equal? (first known-values) name)
	 (get-value (bf known-values)))
	(else (lookup name (skip-value known-values)))))

(define (get-value stuff)
  (if (equal? (first stuff) '!)
      '()
      (se (first stuff) (get-value (bf stuff)))))

(define (skip-value stuff)
  (if (equal? (first stuff) '!)
      (bf stuff)
      (skip-value (bf stuff))))

(define (add name value known-values)
  (if (empty? name)
      known-values
      (se known-values name value '!)))

; ==============================================================================
; ==============================================================================


; EXERCISES

; Exercises about Using the Pattern Matcher

; 16.1

; Design and test a pattern that matches any sentence containing
; the word C three times (not necessarily next to each other).


(match '(* c * c * c *) '(c and c other word c))
(match '(* c * c * c *) '(c c c))
(match '(* c * c * c *) '(c c))

; 16.2

; Design and test a pattern that matches a sentence consisting
; of two copies of a smaller sentence, such as (a b a b).

(match '(&twice &twice) '(a b a b))
(match '(&twice &twice) '(a b c a b c))
(match '(&twice &twice) '(a b c d e f))

; 16.3

; Design and test a pattern that matches any sentence of no more than three words.

(match '(!one !two !three) '(three words here))
(match '(!one !two !three) '(more than three words here))
(match '(!one !two !three) '(less here))

; 16.4

; Design and test a pattern that matches any sentence of at least three words.

(match '(!one !two &three) '(this should match))
(match '(!one !two &three) '(this should also match))
(match '(!one !two &three) '(no match))

; 16.5

; Show sentences of length 2, 3, and 4 that match the pattern (*x *y *y *x)

; length 2
(match '(*x *y *y *x) '(this this))

; length 3

; No match possible -  if one inidivdual letter-named pattern is matched then both are,
; or in other words itt is impossible to match only one of the x's or y's

; length 4
(match '(*x *y *y *x) '(that this this that))

; 16.6

; Show sentences of length 2, 3, and 4 that match the pattern (*x *y &y &x)

; length 2

; No match possible - there must be at least 2 different patterns, &x and &y,
; and if either is matched then the other instance of the pattern will also match

; length 3
(match '(*x *y &y &x) '(a b a))
; Not possible, if &x and &y both match with at least one word, then *x and *y will also match with the words 

; length 4
(match '(*x *y &y &x) '(that this this that))

; 16.7

; List all the sentences of length 6 or less, starting with a b a, that match the pattern

#|
'(a b a b)
'(a b a a b a)
'(a b a b a a)
|#

; Exercises about Implemetation

; 16.8

#|
If the sentence is empty, longest match will test if the minimun number of matches is 0,
if so, it will proceed to test the pattern, adding the empty sentence '() to known values,
otherwise, it returns 'failed.


|#



