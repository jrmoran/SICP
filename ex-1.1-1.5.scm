; # Exercise 1.1:
; Below is a sequence of expressions. What is the result printed
; by the interpreter in response to each expression?
;
; Assume that the sequence is to be evaluated in the order
; in which it is presented.

10                                      ; 10
(+ 5 3 4)                               ; 12
(- 9 1)                                 ; 8
(/ 6 2)                                 ; 3
(+ (* 2 4) (- 4 6))                     ; 6
(define a 3)                            ; N/A
(define b (+ a 1))                      ; N/A
(+ a b (* a b))                         ; 19
(= a b)                                 ; #f
(if (and (> b a) (< b (* a b)))
  b
  a)                                    ; 3
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))                        ; 16
(+ 2 (if (> b a) b a))                  ; 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))                             ; 16

; # Exercise 1.2:
; Translate the following expression into prefix form.
;
;     5 + 4 + (2 − (3 − (6 + 4/5))) / 3(6 − 2)(2 − 7)
;
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))                ; -37/150

; # Exercise 1.3:
; Define a procedure that takes three numbers as arguments and returns
; the sum of the squares of the two larger numbers.

; solution:

(define (sqr x) (* x x))

(define (sum-sqr x y)
  (+ (sqr x) (sqr y)))

(define (larger x y)
  (if (> x y) x y))

(define (lesser x y)
  (if (> x y) y x))

(define (ex-1.3 a b c)
  (sum-sqr (larger a b)
           (larger (lesser a b) c)))

(ex-1.3 5 2 3)            ; 34
(ex-1.3 2 0 1)            ; 5
(ex-1.3 3 4 5)            ; 41

; # Exercise 1.4:
; Observe that our model of evaluation allows for combinations whose
; operators are compound expressions. Use this observation to describe
; the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; solution:

; behavior using applicative order evaluation. If the predicate in the
; special `if` form evaluates to true, the operator `+` is applied,
; otherwise the `-` operator is applied to the formal parameters.

(a-plus-abs-b 4 5)
((if (> 5 0) + -) 4 5)
(+ 4 5)
9

; # Exercise 1.5:
; Ben Bitdiddle has invented a test to determine whether the interpreter
; he is faced with is using applicative-order evaluation or normal-order
; evaluation. He defines the following two procedures:

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

; Then he evaluates the expression

(test 0 (p))

; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe with
; an interpreter that uses normal-order evaluation? Explain your answer.
; (Assume that the evaluation rule for the special form if is the same
; whether the interpreter is using normal or applicative order: The
; predicate expression is evaluated first, and the result determines
; whether to evaluate the consequent or the alternative expression.)

; Solution
;
; With applicative order evaluation `(p)` will continue to evaluate to
; itself indefinitely.

(test 0 (p))
(test 0 (p))
(test 0 (p))

; While on normal order evaluation, compound expressions will be
; expanded until we only have primitives

(test 0 (p))
(if (= 0 0) 0 (p))
(#t 0 (p))

; Since the predicate evaluates to true, the alternative expression
; `(p)` is never evaluated.
