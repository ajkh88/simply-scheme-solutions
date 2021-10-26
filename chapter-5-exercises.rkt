#lang simply-scheme

#|

Boring Exercises

5.1

(sentence 'I '(me mine))

Result: '(I me mine)

(sentence '() '(is empty))

Result: '(is empty)

(word '23 '45)

Result: '2345

(se '23 '45)

Result: '(23 45)

(bf 'a)

Result: ""

(bf '(aye))

Result: '()

(count (first '(maggie mae)))

Result: 6

(se "" '() "" '())

Result: '("" "")

(count (se "" '() "" '()))

Result: 2
|#
; 5.2

(define (f1 s1 s2)
  (se (bf s1)
      (bl s2)))
(f1 '(a b c) '(d e f))
; Expect (B C D E)

(define (f2 s1 s2)
  (se (bf s1)
      (bl s2)
      (word (first s1) (last s2))))
(f2 '(a b c) '(d e f))
; Expect (B C D E AF)

(define (f3 s1 s2)
  (se s1 s1))
(f3 '(a b c) '(d e f))
; Expect (A B C A B C)

(define (f4 s1 s2)
  (word (item 2 s1)
        (item 2 s2)))
(f4 '(a b c) '(d e f))
; Expect BE

#|

5.3

The first is a word and made of letters, and so first will return the first letter, 'm,
the second is a sentence made of words, and so first will return the first word, so 'mezzanine

5.4

The first will call the procedure square on 7 and get 49, and then call the first function on the result to get the first 'item', in this case 4
The second will call the firt function on the sentence '(square 7), returning 'square

5.5

The first will create and return a word from the three arguments, returning 'abc
The second will create and return a sentence from the three arguments, returning '(a b c)

5.6

The only different is that the function name is abbreviated, the function itself is the same

5.7

The first calls the butfirst (with name abbreviated) function on the word 'x, returning the empty string "",
The second calls the butfirst function on the sentence '(x), returning the empty sentence '()

5.8

(here, there and everywhere) 
(help!) - legal
(all i've got to do)
(you know my name (look up the number)) - legal

5.9

'mattwright
'brianharvey

5.10

To return a word, pass a word to butfirst
To return a sentence, pass a sentence to butfirst

5.11

To return a word, pass a word or sentence to last
To return a sentence, pass a sentece of sentences

5.12

(first) cannot return empty word
(last) cannot return empty word
(butfirst) can return an empty word by passing a single letter long word, and an empty sentence by passing asentence with one word
(butlast) can return an empty word by passing a single letter long word, and an empty sentence by passing asentence with one word


|#

#|

Real exercises

5.13

The ' is shorthand for the quote function, so this calls (quote) on the word 'banana, and returns the double-quoted word banana.
(first ' 'banana) returns the identity of the first result, i.e. the (quote) function
|#


; 5.14

(define (third collection)
  (item 3 collection))

(third '(I me mine))

; 5.15
(define (first-two w)
  (if (word? w)
      (word (first w) (first (bf w)))
      ""))

(first-two 'alexander)

; 5.16

(define (two-first w1 w2)
  (if (and (word? w1) (word w2))
      (word (first w1) (first w2))
      ""))

(two-first 'brian 'epstein)

(define (two-first-sent s)
  (word (first (item 1 s)) (first (item 2 s))))
(two-first-sent '(brian epstein))

; 5.17

(define (knight name)
  (se 'Sir name))

(knight '(Alex Hughes))

#| 5.18

(define (ends word)
  (word (first word) (last word)))

(ends 'john)

The name word is already bound to a function, but this is overriden in the scope of the
function when passed as an argument, so the word function is no longer accessible and the
function tries to apply the argument as a function. If it is not a function (as in this case)
then it fails.

|#

; 5.19

(define (insert-and sent)
  (se (bl sent) 'and (last sent)))

(insert-and '(john bill wayne fred joey))

; 5.20

(define (middle-names names)
  (bf (bl names)))

(middle-names '(james paul mccartney))
(middle-names '(john ronald raoul tolkein))

; 5.21

(define (query quest)
  (se (first (bf quest))
      (first quest)
      (bf (bf (bl quest)))
      (word (last quest)'?)))

(query '(you are experienced))
(query '(I should have known better))

