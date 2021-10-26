#lang simply-scheme


; Boring Exercises

; 13.1

(define (explode wd)
  (if (empty? wd)
      '()
      (se (first wd) (explode (bf wd)))))

(trace explode)

(explode 'ape)

(untrace explode)


; 13.2

(define (pigl wd)
  (if (member? (first wd) 'aeiou)
      (word wd 'ay)
      (pigl (word (bf wd) (first wd)))))

(trace pigl)

(pigl 'throughout)

(untrace pigl)

#|
1. takes 'throughout, passes 'hroughoutt to
2. takes 'hroughoutt, passes 'roughoutth to
3. takes 'roughoutth, passes 'oughoutthr to
4. takes 'oughoutthr, adds 'ay and returns it
|#

; 13.3

(define (downup wd)
  (se wd (downup (bl wd)) wd))

(trace downup)

; (downup 'toe)

(untrace downup)

#|

As there is no base case, the function repeatedly calls itself on a smaller version of the input,
eventually getting down an empty string that returns an error.
|#

; 13.4

(define (forever n)
  (if (= n 0)
      1
      (+ 1 (forever n))))

(trace forever)

(forever -5)

(untrace forever)

#|
The base case expects that 0 will eventually be reached, however the function adds 1 to n before calling itself,
meaning that 0 can never be reached, as the base case returns 1, so even if a value less than 1 is passed,
when0  is reached, 1 is returned and the function keeps adding 1 to n ad infinitum.
|#

; Real exercises

; 13.5


#|
In the case of pigl, the recursion doesn't 'unwind' - when the base case is reached then 'ay is added and
the word is returned. The second branch of the if evaluates the function itself, the result is not needed by
the previous calls. In the case of downup, a sentence is made from the argument and the result of the recursive
call, meaning that the function 'unwinds' and the result of each recursive call is passed back up the chain.
|#

; 13. 6

#|
The recursive call is made as part of the if expression, whcih returns the value it evaluates to. Each
function is called subsequently by the previous one and as such a value must be returned to the caller.
|#


