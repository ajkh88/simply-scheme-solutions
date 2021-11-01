#lang simply-scheme

; Boring Exercises

; 19.1  What happens if you say the following?

; (every cdr '((john lennon) (paul mccartney)
;            (george harrison) (ringo starr)))

; How is this different from using map, and why? How about cadr instead of cdr? 

 (every cdr '((john lennon) (paul mccartney)
            (george harrison) (ringo starr)))
 (map cdr '((john lennon) (paul mccartney)
            (george harrison) (ringo starr)))
 (map cadr '((john lennon) (paul mccartney)
            (george harrison) (ringo starr)))

#|

The every procedure uses (se) to join the results of the called argument function. This
essentially removes the nested lists and joins them into a single list.
Map applies the procedure to the each element and uses cons to create a list, meaning that
the sublists are still represented. Using (cadr) as the argument to map means that a word
is returned and therefore when cons is applied to the returned values they are joined into
a non-structured list.

|#


; Real Exercises

; 19.2

; Write keep. Don't forget that keep has to return a sentence if its second argument
; is a sentence, and a word if its second argument is a word.

; (Hint: it might be useful to write a combine procedure that uses either word or sentence depending on the types of its arguments.) 


(define (keep pred sent)
  (if (word? sent)
    (combine pred sent word)
    (combine pred sent se)))

(define (combine pred sent combiner)
  (cond ((empty? sent) (combiner))
        ((pred (first sent)) (combiner (first sent) (combine pred (butfirst sent) combiner)))
        (else (combine pred (butfirst sent) combiner))))

(keep (lambda (x) (> x 10)) '(12 10 4 5 14 15))
(keep (lambda (x) (= x 4)) '1210451415)

; 19.3

; Write the three-argument version of accumulate that we described.

(define (three-arg-accumulate fn identity lst)
  (if (empty? lst)
      identity
      (fn (car lst) (three-arg-accumulate fn identity (cdr lst)))))

(three-arg-accumulate + 0 '(4 5 6))
(three-arg-accumulate + 0 '())
(three-arg-accumulate cons '() '(a b c d e))

; 19.4

; Our accumulate combines elements from right to left. That is,

; (accumulate - '(2 3 4 5))

; computes 2-(3-(4-5)). Write left-accumulate, which will compute ((2-3)-4)-5 instead.
; (The result will be the same for an operation such as +, for which grouping order
; doesn't matter, but will be different for -.)

(define (left-accumulate fn lst)
  (if (empty? (cdr lst))
      (car lst)
      (fn (left-accumulate fn (bl lst)) (last lst))))

(reduce - '(2 3 4 5))
(left-accumulate - '(2 3 4 5))
(reduce + '(2 3 4 5))
(left-accumulate + '(2 3 4 5))

; 19.5

; Rewrite the true-for-all? procedure from Exercise 8.10.
; Do not use every, keep, or accumulate.

(define (true-for-all? pred lst)
  (= (length lst) (length (filter pred lst))))
(show "19.5")
(true-for-all? even? '(2 4 6 8))
(true-for-all? even? '(2 6 3 4))

; 19.6

; Write a procedure true-for-any-pair? that takes a predicate and
; a sentence as arguments. The predicate must accept two words as
; its arguments. Your procedure should return #t if the argument
; predicate will return true for any two adjacent words in the sentence: 

(define (true-for-any-pair? pred lst)
  (cond ((< (length lst) 2) #f)
        ((pred (car lst) (cadr lst)) #t)
        (else (true-for-any-pair? pred (cdr lst)))))

(show "19.6")
(true-for-any-pair? equal? '(a b c b a))
(true-for-any-pair? equal? '(a b c c d))
(true-for-any-pair? < '(20 16 5 8 6))

; 19.7

; Write a procedure true-for-all-pairs? that takes a predicate and a
; sentence as arguments. The predicate must accept two words as its
; arguments. Your procedure should return #t if the argument predicate
; will return true for every two adjacent words in the sentence:

(define (true-for-all-pairs? pred lst)
  (cond ((< (length lst) 2) #t)
        (else (and (tfallp-helper pred lst)
              (true-for-all-pairs? pred (cdr lst))))))
  
(define (tfallp-helper pred lst)
  (cond ((< (length lst) 2) #t)
        ((pred (car lst) (cadr lst)) (tfallp-helper pred (cdr lst)))
        (else #f)))
(show "19.7")
(true-for-all-pairs? equal? '(a b c c d))
(true-for-all-pairs? equal? '(a a a a a))
(true-for-all-pairs? < '(20 16 5 8 6))
(true-for-all-pairs? < '(3 7 19 22 43))

; 19.8

; Rewrite true-for-all-pairs? (Exercise 19.7) using true-for-any-pair?
; (Exercise 19.6) as a helper procedure. Don't use recursion in solving
; this problem (except for the recursion you've already used to write
; true-for-any-pair?). Hint: You'll find the not procedure helpful. 

(define (true-for-all-pairs?-2 pred lst)
  (not (true-for-any-pair? (lambda (a b) (not (pred a b))) lst)))

(show "19.8")
(true-for-all-pairs?-2 equal? '(a b c c d))
(true-for-all-pairs?-2 equal? '(a a a a a))
(true-for-all-pairs?-2 < '(20 16 5 8 6))
(true-for-all-pairs?-2 < '(3 7 19 22 43))

;  19.9

; Rewrite either of the sort procedures from Chapter 15 to take two arguments,
; a list and a predicate. It should sort the elements of that list according
; to the given predicate:


(define (sort sent pred)
  (if (<= (count sent) 1)
      sent
      (merge (sort (one-half sent) pred)
             (sort (other-half sent) pred) pred)))

(define (merge left right pred)
  (cond ((empty? left) right)
	((empty? right) left)
	((pred (first left) (first right))
	 (se (first left) (merge (bf left) right pred)))
	(else (se (first right) (merge left (bf right) pred)))))

(define (one-half sent)
  (if (<= (count sent) 1)
      sent
      (se (first sent) (one-half (bf (bf sent))))))

(define (other-half sent)
  (if (<= (count sent) 1)
      '()
      (se (first (bf sent)) (other-half (bf (bf sent))))))

(sort '(4 23 7 5 16 3) <)
(sort '(4 23 7 5 16 3) >)
(sort '(john paul george ringo) before?)

; 19.10

; Write tree-map, analogous to our deep-map, but for trees,
; using the datum and children selectors.

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (leaf? node)
  (null? (children node)))

(define test-tree-1
  (make-node
   1
   (list (make-node 2
                    (list (make-node 4 '())
                          (make-node 5 '())))
         (make-node 3
                    (list (make-node 6 '())
                          (make-node 7 '()))))))

(define (tree-map f tree)
  (if (leaf? tree)
      (make-node (f (datum tree)) '())
      (cons (f (datum tree))
            (forest-map f (children tree)))))

(define (forest-map f forest)
  (if (null? forest)
      '()
      (cons (tree-map f (car forest))
            (forest-map f (cdr forest)))))

(tree-map (lambda (x) (* 2 x)) test-tree-1)

; 19.11  Write repeated. (This is a hard exercise!)

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated-new f times)
  (if (= 1 times)
      (lambda (lst) (f lst))
      (compose f (repeated-new f (- times 1)))))


((repeated-new bf 3) '(she came in through the bathroom window))
((repeated-new (lambda (x) (* x x)) 2) 3)

; 19.12  Write tree-reduce. You may assume that the combiner
; argument can be invoked with no arguments.

(define (tree-reduce f tree)
  (if (leaf? tree)
      (datum tree)
      (f (datum tree)
         (forest-reduce f (children tree)))))

(define (forest-reduce f forest)
  (if (null? forest)
      (f)
      (f (tree-reduce f (car forest))
         (forest-reduce f (cdr forest)))))

   
(tree-reduce
 +
 (make-node 3 (list (make-node 4 '())
                    (make-node 7 '())
                    (make-node 2 (list (make-node 3 '())
                                       (make-node 8 '()))))))

; 19.13

; Write deep-reduce, similar to tree-reduce, but for structured lists:

(define (deep-reduce f structure)
  (cond ((null? structure) (f))
        ((list? (car structure))
         (f (deep-reduce f (car structure))
                 (deep-reduce f (cdr structure))))
        (else (f (car structure) (deep-reduce f (cdr structure))))))

  
(deep-reduce word '(r ((a (m b) (l)) (e (r)))))



