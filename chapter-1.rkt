#lang simply-scheme
; SIMPLY SCHEME
; Chapter 1

(define (choices menu)
    (if (null? menu)
        '(())
        (let ((smaller (choices (cdr menu))))
          (reduce append
                  (map (lambda (item) (prepend-every item smaller))
                       (car menu))))))
(define (prepend-every item lst)
    (map (lambda (choice) (se item choice)) lst))

;(choices '((small medium large)
;             (vanilla (ultra chocolate) (rum raisin) ginger)
;            (cone cup)))


(define (combinations size set)
  (cond ((= size 0) '(()))
	((empty? set) '())
	(else (append (prepend-every (first set)
				     (combinations (- size 1)
						   (butfirst set)))
		      (combinations size (butfirst set))))))

;(combinations 3 '(a b c d e))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

;(factorial 4)