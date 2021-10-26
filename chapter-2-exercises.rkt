#lang simply-scheme

;Utility functions

(define (vowel? letter)
  (if (and (word? letter) (eq? (count letter) 1))
      (member? letter 'aeiou)
      #f))

#|

2.1

______________________________________________________
| function | arg 1       | arg 2            | result |
|-----------------------------------------------------
| word     | now         | here             | nowhere
| sentence | now         | here             | (now here)
| first    | blackbird   | none             | b
| first    | (blackbird) | none             | blackbird
| +        | 3           | 4                | 7
| every    | butfirst    | (thank you girl) | (hank ou irl)
| member?  | e           | aardvark         | #f
| member?  | the         | (the long road)  | #t
| keep     | vowel?      | (i will)         | (i)
| keep     | vowel?      | predilection     | eieio
| last     | ()          | none             | Invalid
| every    | last        | (honey pie)      | (y e)
| keep     | vowel?      | taxman           | aa
-----------------------------------------------

(word 'now 'here)
(sentence 'now 'here)
(first 'blackbird)
(first '(blackbird))
(+ 3 4)
(every butfirst '(thank you girl))
(member? 'e 'aardvark)
(member? 'the '(the long road))
(keep vowel? 'predilection)
(last "")
(every last '(honey pie))
(keep vowel? 'taxman)


2.2

All words of length 1

2.3

appearance:
  domain: the first argument takes any word, sentence or number, and the second argument takes any word sentence or number
  range: an integer of value zero or higher 

2.4

item:
  domain: the first argument must be a positive integer greater than 0 and less than the total length of the second argument, the second argument must be a sequence of length at least 1
  range: a member of the sequence passed in the second argument, which could be a number, word or sentence



1 Arg

(cos)
(expt)
(not)
(random)
(round)
(sqrt)

2 Arg

(<=)
(<)
(=)
(>=)
(>)
(appearances)
(equal?)
(every)
(item)
(keep)
(member?)
(sentence)

Special
(if)

2.5
(count
(even?)
(number-of-arguments)
(number?)
(odd?)
(vowel?)

2.6
(butfirst)
(butfirst)
(butlast)
(first)
(last)

2.7

(+)
(-)
(*)
(/)
(and)
(or)
(max)
(quotient)
(remainder)
(word)

2.8

(+)
(*)
(and)
(or)
(max)

2.9

(fn a (fn b c) = (fn (fn a b) c)

(+)
(*)
(-)
(and)
(or)
(max)
(word)


|#


