#lang simply-scheme

(define (vowel? l) (member? l 'aeiou))

#|

Boring Exercises

8.1

> (every last '(algebra purple spaghetti tomato gnu))
'(a e i o u)

> (keep number? '(one two three four))
'()

> (accumulate * '(6 7 13 0 9 42 17))
0

> (member? 'h (keep vowel? '(t h r o a t)))
#f

> (every square (keep even? '(87 4 7 12 0 5)))
'(16 144 0)

> (accumulate word (keep vowel? (every first '(and i love her))))
'ai

> ((repeated square 0) 25)
25

> (every (repeated bl 2) '(good day sunshine))
'(go d sunshi)

8.2

> (keep vowel? 'birthday)
IA

> (every first '(golden slumbers))
(G S)

> (first '(golden slumbers))
GOLDEN

> (every last '(little child))
(E D)

> (accumulate word (every last '(little child)))
ED

> (every + '(2 3 4 5))
(2 3 4 5)

> (accumulate + '(2 3 4 5))
14


8.3

(define (f a)
  (keep even? a))

f will keep every element in a that is even, i.e. evenly divisible by 2,
returning a sentence containing them

(define (g b)
  (every b '(blue jay way)))

g will apply the function passed as b to the every element of the sentence '(blue jay way),
returning the result as a sentence

(define (h c d)
  (c (c d)))
h takes a function as argument c, and applies it twice to the argument d

(define (i e)
  (/ (accumulate + e) (count e)))
i takes a collection as e, adds all elements together and divides it by the number of elements - it calcualtes the mean

accumulate
accumulate takes a function and a collection as arguments. It applies the function to each element in turn,
with the result of the function application to the last element. At the end the result of the repeated function application is returned.

sqrt
Tis function takes a single number as an argument and returns the square root of it.

repeated
takes a function and an integer as argument,
and returns a function that applies the function the nmber of times specified in the second argument.

(repeated sqrt 3)
This returns a function that applies the square root function 3 times in turn,
each return value feeding into the next application of the sqrt function.

(repeated even? 2)
This returns a function that applied the even? predicate twice - will probably fail as even? cannot be applied to a boolean?

(repeated first 2)
This returns a function that applies first twice to the argument. This will return the first element,
as repeateed application has no effect when there is only one element.

(repeated (repeated bf 3) 2)
This returns a function that twice applies the result of another repeated procedure. The first returns a function that calls bf 3 times, which is in turn applied twice.
In totla bf will be applied 6 times, effectively removing the first 6 elements of the collection.


|#


; Real exercises

; 8.4


(define (choose-beatles pred)
  (keep pred '(john paul george ringo)))

(define (ends-vowel? wd) (vowel? (last wd)))

(define (even-count? wd) (even? (count wd)))

(choose-beatles ends-vowel?)
(choose-beatles even-count?)

; 8.5

(define (transform-beatles proc)
  (every proc '(john paul george ringo)))

(define (amazify name)
  (word 'the-amazing- name))

(transform-beatles amazify)
(transform-beatles butfirst)

; 8.6

(define (phonetic l)
  (cond ((eq? l 'a) 'alpha)
        ((eq? l 'b) 'bravo)
        ((eq? l 'c) 'charlie)
        ((eq? l 'd) 'delta)
        ((eq? l 'e) 'echo)
        ((eq? l 'f) 'foxtrot)
        ((eq? l 'g) 'golf)
        ((eq? l 'h) 'hotel)
        ((eq? l 'i) 'india)
        ((eq? l 'j) 'juliet)
        ((eq? l 'k) 'kilo)
        ((eq? l 'l) 'lima)
        ((eq? l 'm) 'mike)
        ((eq? l 'n) 'november)
        ((eq? l 'o) 'oscar)
        ((eq? l 'p) 'papa)
        ((eq? l 'q) 'quebec)
        ((eq? l 'r) 'romeo)
        ((eq? l 's) 'sierra)
        ((eq? l 't) 'tango)
        ((eq? l 'u) 'uniform)
        ((eq? l 'v) 'victor)
        ((eq? l 'w) 'whiskey)
        ((eq? l 'x) 'x-ray)
        ((eq? l 'y) 'yankee)
        ((eq? l 'z) 'zulu)
        (else 'wot)))

(define (words w)
  (every phonetic w))

(words 'cab)

; 8.7

(define (letter-count sent)
  (accumulate + (every count sent)))

(letter-count '(fixing a hole))

; 8.8

(define (exaggerate-helper word)
  (cond ((number? word) (* word 2))
        ((eq? word 'good) 'great)
        ((eq? word 'bad) 'terrible)
        (else word)))

(define (exaggerate sent)
  (every exaggerate-helper sent))

(exaggerate '(i ate 3 potstickers))
(exaggerate '(the chow fun is good here))

; 8.9

(every word '(i am the walrus 9))

(keep word? '(8 days a week))
(keep (lambda (n) #t) '(this is a sentence))

(accumulate se '(revolution number 9))

; 8.10

(define (true-for-all? pred sent)
  (= (count sent) (count (keep pred sent))))

(true-for-all? even? '(2 4 6 8))
(true-for-all? even? '(2 6 3 4))


; 8.11
(define (base-grade grade)
  (cond ((eq? 'a grade) 4)
        ((eq? 'b grade) 3)
        ((eq? 'c grade) 2)
        ((eq? 'd grade) 1)
        (else 0)))

(define (grade-modifier grade)
  (let ((base (base-grade (item 1 grade)))
        (third 0.33))
  (if (= 2(count grade))
      (if (eq? '- (item 2 grade))
          (- base third)
          (+ base third))
      base)))
  
(define (gpa grades)
   (/ (accumulate + (every grade-modifier grades)) (count grades)))

(gpa '(a a+ b+ b))

; 8.12

(define (is-um? wd)
  (eq? wd 'um))

(define (count-ums sent)
  (count (keep is-um? sent)))

(count-ums
   '(today um we are going to um talk about functional um programming))

; 8.13

(define (letter-to-num l)
  (cond ((member? l 'abc) 2)
        ((member? l 'def) 3)
        ((member? l 'ghi) 4)
        ((member? l 'jkl) 5)
        ((member? l 'mno) 6)
        ((member? l 'pqrs) 7)
        ((member? l 'tuv) 8)
        ((member? l 'wxyz) 9)
        (else '*)))

(define (phone-unspell wordnum)
  (accumulate word (every letter-to-num wordnum)))

(phone-unspell 'popcorn)


; 8.14


(define (subword wd start end)
    ((repeated bf (- start 1)) ((repeated bl (- (count wd) end)) wd)))

(subword 'polythene 5 8)
(subword 'programming 4 7)
