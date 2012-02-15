; # Exercise 1.6:
; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
; "Why can’t I just define it as an ordinary procedure in terms of cond?"
; she asks. Alyssa's friend Eva Lu Ator claims this can indeed be done,
; and she defines a new version of if:
;
(define (new-if predicate then-clause else-clause)
 (cond (predicate then-clause)
       (else else-clause)))
;
; Eva demonstrates the program for Alyssa:
;
;     (new-if (= 2 3) 0 5)
;     5
;     (new-if (= 1 1) 0 5)
;     0
;
; Delighted, Alyssa uses new-if to rewrite the square-root program:
;
;     (define (sqrt-iter guess x)
;       (new-if (good-enough? guess x)
;               guess
;               (sqrt-iter (improve guess x)
;                           x)))
;
; What happens when Alyssa attempts to use this to compute square roots? Explain.
;
; Solution:
;
; The special `if` form has its own order of evaluation, first the
; predicate is evaluated and depending the result, either the consequent
; or alternate expressions are evaluated. The `new-if` procedure is
; evaluated using applicative order, so all expressions are evaluated.
; Alyssa's new if evaluates both consequent and alternate expressions
; regardless of the predicate expression value, so the procedure
; `sqrt-iter` continues evaluating itself indefinitely.

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (square x) (* x x))

(define (sqrt2 x) (sqrt-iter 1.0 x))

(sqrt2 2)    ; 1.4142156862745097

(sqrt2 9)    ; 3.00009155413138


(define (sqrt-iter-2 guess x)
  "This runs perfectly using Racket's R5RS and Gambit-Scheme"
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt2-2 x) (sqrt-iter-2 1.0 x))

(sqrt2-2 9)

; # Exercise 1.7:
; The good-enough? test used in computing square roots will not be very effective
; for finding the square roots of very small numbers. Also, in real computers,
; arithmetic operations are almost always performed with limited precision.
;
; This makes our test inadequate for very large numbers. Explain these statements,
; with examples showing how the test fails for small and large numbers.
;
; An alternative strategy for implementing good-enough? is to watch how guess
; changes from one iteration to the next and to stop when the change is a very
; small fraction of the guess.
;
; Design a square-root procedure that uses this kind of end test. Does this
; work better for small and large numbers?
;
; Solution:
;
; First some procedures to help us see what's going on
;

(define (put x y)
  (display (string-append (number->string x) " "
                          (number->string y)))
  (newline))

(define (good-enough? guess x)
  (let ((diff (abs (- (square guess) x)))
        (tolerance 0.001))
    (put guess diff)
    (< diff tolerance)))

(sqrt2 2)

(sqrt2 0.0001)

(sqrt2 0.00001)

(sqrt2 1e35)   ; never ending process

; The procedure good-enough? uses 0.001 as a tolerance quotient, this is
; quite large for smaller numbers than 0.001, for larger numbers than
; 1e30 (on my system) the procedure never terminates. The guess never
; goes below the tolerance quotient.
;
; for example
;
;        x            expected          obtained
;     -------    -------------     -------------
;      0.0001             0.01          0.032308
;     0.00001         0.003162          0.031356
;        1e35     3.162277e+17     process never
;                                     terminates
;
; If we change the tolerance quotient to 0.00000001 we get better
; results for smaller numbers, but not larger numbers.
;

; Instead, we can evaluate if a guess is good enough based on how
; different a new guess is from the previous guess.

(define (good-enough? guess x)
  (let ((diff (/ (abs (- (improve guess x) guess)) guess))
        (tolerance 0.001))
    (< diff tolerance)))

(sqrt2 0.0001)    ; 0.01

(sqrt2 0.00001)   ; 0.003

(sqrt2 1e35)      ; 3.1622e17

; # Exercise 1.8:
; Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is given
; by the value
;
;     x /y^2 + 2y
;     -----------
;          3
;
; Use this formula to implement a cube-root procedure analogous to the
; square-root procedure.
;
; Solution:
;
; in scheme we can get the cube root of 8 like this

(expt 8 (/ 1.0 3))        ; 2

; First we need a procedure to improve our guesses and another to
; iterate over over all improve guesses until we get good enough one.
(define (improve-cub guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess)) 
     3))

(define (cube-iter guess x)
  (if (good-enough? guess x)
    guess
    (cube-iter (improve-cub guess x) x)))


; good enough just needs to compare the guess to the power of 3 against
; the value of x
(define (good-enough? guess x)
  (< (abs (- (cube-pow guess) x)) 0.001))

(define (cube-pow x) (* x x x))

(define (cube x) (cube-iter 1.0 x))

(cube 8)     ; 2.000004
(cube 27)    ; 3.000000
(cube 64)    ; 4.000017
