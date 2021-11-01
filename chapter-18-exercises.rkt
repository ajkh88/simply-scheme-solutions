#lang simply-scheme

;; Tree-related procedures

(define (make-node datum children)
  (cons datum children))

(define (datum node)
  (car node))

(define (children node)
  (cdr node))

(define (leaf? node)
  (null? (children node)))

(define (leaf datum)
  (make-node datum '()))

(define (cities name-list)
  (map leaf name-list))

(define world-tree
  (make-node
   'world
   (list (make-node
          'italy
          (cities '(venezia riomaggiore firenze roma)))
         (make-node
          '(united states)
          (list (make-node
                 'california
                 (cities '(berkeley (san francisco) gilroy)))
                (make-node
                 'massachusetts
                 (cities '(cambridge amherst sudbury)))
                (make-node 'ohio (cities '(kent)))))
         (make-node 'zimbabwe (cities '(harare hwange)))
         (make-node 'china
		    (cities '(beijing shanghai guangzhou suzhou)))
         (make-node
          '(great britain)
          (list 
           (make-node 'england (cities '(liverpool)))
           (make-node 'scotland
		      (cities '(edinburgh glasgow (gretna green))))
           (make-node 'wales (cities '(abergavenny)))))
         (make-node
          'australia
          (list
           (make-node 'victoria (cities '(melbourne)))
           (make-node '(new south wales) (cities '(sydney)))
           (make-node 'queensland
		      (cities '(cairns (port douglas))))))
         (make-node 'honduras (cities '(tegucigalpa))))))

(define test-tree-1
  (make-node
   'a
   (list (make-node 'b
                    (list (make-node 'd '())
                          (make-node 'e '())))
         (make-node 'c
                    (list (make-node 'f '())
                          (make-node 'g '()))))))

; 18.1

; What does

; ((SAN FRANCISCO))

; mean in the printout of world-tree? Why two sets of parentheses?

#|

As 'San Fransisco' is two words, we need to declare is using sub-list,
otherwise 'San' and 'Fransisco' would be separately decalred as cities under
'California'

|#

; 18.2


; Suppose we change the definition of the tree constructor so that it uses list instead of cons:

; (define (make-node datum children)
;   (list datum children))

; How do we have to change the selectors so that everything still works?

#|

If we change make-noe to use list, then children would be a sublist, so the call to
cdr in (children) would return a list of a list, rather than a list. We could update this
to use cadr to solve this.

|#

; 18.3

; Write depth, a procedure that takes a tree as argument and returns the
; largest number of nodes connected through parent-child links.
; That is, a leaf node has depth 1; a tree in which all the children of
; the root node are leaves has depth 2.
; Our world tree has depth 4 (because the longest path from the root
; to a leaf is, for example, world, country, state, city).


(define (depth tree)
  (if (leaf? tree)
      1
      (+ 1 (depth-of-forest (children tree)))))
     
(define (depth-of-forest forest)
  (if (null? forest)
      0
      (max (depth (car forest))
           (depth-of-forest (cdr forest)))))

(depth world-tree)
(depth test-tree-1)

; 18.4

; Write count-nodes, a procedure that takes a tree as argument and returns
; the total number of nodes in the tree. (Earlier we counted the number of leaf nodes.)


(define (count-nodes tree)
  (if (leaf? tree)
      1
      (+ 1 (count-nodes-of-forest (children tree)))))
     
(define (count-nodes-of-forest forest)
  (if (null? forest)
      0
      (+ (count-nodes (car forest))
         (count-nodes-of-forest (cdr forest)))))

(count-nodes test-tree-1)
(count-nodes world-tree)
(count-nodes (make-node 'root '()))

    
; 18.5
; Write prune, a procedure that takes a tree as argument and returns a copy
; of the tree, but with all the leaf nodes of the original tree removed.
; (If the argument to prune is a one-node tree, in which the root node has
; no children, then prune should return #f because the result of removing the
; root node wouldn't be a tree.) 


(define (prune tree)
  (if (leaf? tree)
      #f
      (make-node (datum tree) (prune-forest (children tree)))))

(define (prune-forest forest)
  (cond ((null? forest) '())
        ((leaf? (car forest)) (prune-forest (cdr forest)))
        (else (make-node (prune (car forest)) (prune-forest (cdr forest))))))

(prune test-tree-1)
(prune (make-node 'root '()))

; 18.6
; Write a program parse-scheme that parses a Scheme arithmetic expression
; into the same kind of tree that parse produces for infix expressions.
; Assume that all procedure invocations in the Scheme expression have two arguments.

; The resulting tree should be a valid argument to compute:

; > (compute (parse-scheme '(* (+ 4 3) 2)))
; 14

; (You can solve this problem without the restriction to two-argument
; invocations if you rewrite compute so that it doesn't assume every branch
; node has two children.)



(define (compute tree)
  (if (number? (datum tree))
      (datum tree)
      ((function-named-by (datum tree))
         (compute (car (children tree)))
         (compute (cadr (children tree))))))

(define (function-named-by oper)
  (cond ((equal? oper '+) +)
	((equal? oper '-) -)
	((equal? oper '*) *)
	((equal? oper '/) /)
	(else (error "no such operator as" oper))))

; The original parser from the chapter also works, but is needlessly complex -
; the same effect can be achieved by arrange the expression as a tree. Scheme
; expressions are already tree-shaped
(define (parse-scheme expr)
  (cond ((null? expr) '())
        ((number? (car expr)) (cons (leaf (car expr)) (parse-scheme (cdr expr))))
        ((list? (car expr)) (cons (parse-scheme (car expr)) (parse-scheme (cdr expr))))
        (else (make-node (car expr) (parse-scheme (cdr expr))))))

(compute (parse-scheme '(+ (- (+ 4 (* 3 7)) (/ 5 (+ 4 3))) 6.0)))
(compute (parse-scheme '(* (+ 4 3) 2)))

