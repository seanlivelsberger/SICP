; Exercises from Chapter 1 of SICP.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.1.  Below is a sequence of expressions. What is the result
; printed by the interpreter in response to each expression? Assume that
; the sequence is to be evaluated in the order in which it is presented.

10 ;10
(+ 5 3 4) ;12
(- 9 1) ;8
(/ 6 2) ;3
(+ (* 2 4) (- 4 6)) ;6
(define a 3) ;a
(define b (+ a 1)) ;b
(+ a b (* a b)) ;19
(= a b) ;false
(if (and (> b a) (< b (* a b)))
    b
    a) ;4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ;16
(+ 2 (if (> b a) b a)) ;6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ;16

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.2.  Translate the following expression into prefix form

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 3))))) (* 3 (- 6 2) (- 2 7)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Exercise 1.3.  Define a procedure that takes three numbers as arguments
; and returns the sum of the squares of the two larger numbers.

(define (p a b c)
	(cond ((and (> a c) (> b c)) (+ (* a a) (* b b)))
		  ((and (> a b) (> c b)) (+ (* a a) (* c c)))
		  (else (+ (* b b) (* c c)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.4.  Observe that our model of evaluation allows for combinations
; whose operators are compound expressions. Use this observation to describe
; the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; The final output is a combination of a and b on some operator, which is
; determined by whether or not b is greater than 0, deciding between + and
; -.

;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Exercise 1.5.  Ben Bitdiddle has invented a test to determine whether the
; interpreter he is faced with is using applicative-order evaluation or
; normal-order evaluation. He defines the following two procedures:

; (define (p) (p))

; (define (test x y)
;   (if (= x 0)
;       0
;       y))

; Then he evaluates the expression

; (test 0 (p))

; What behavior will Ben observe with an interpreter that uses
; applicative-order evaluation? What behavior will he observe with an
; interpreter that uses normal-order evaluation? Explain your answer.
; (Assume that the evaluation rule for the special form if is the same
; whether the interpreter is using normal or applicative order: The
; predicate expression is evaluated first, and the result determines
; whether to evaluate the consequent or the alternative expression.)

; Under normal-order evaluation, the if statement expands quickly,
; evaluating the entire expression to 0, (if (= 0 0) 0 (p) ).
; Under applicative-order evaluation, the inputs to the function will be
; continuously evaluated in an endless loop as the second input (p) will
; never completely evaluate seeing as it is self-referential.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.6.  Alyssa P. Hacker doesn't see why if needs to be provided as
; a special form. ``Why can't I just define it as an ordinary procedure in
; terms of cond?'' she asks. Alyssa's friend Eva Lu Ator claims this can
; indeed be done, and she defines a new version of if:
;
; (define (new-if predicate then-clause else-clause)
;   (cond (predicate then-clause)
;         (else else-clause)))
;
; Eva demonstrates the program for Alyssa:
;
; (new-if (= 2 3) 0 5)
; 5
;
; (new-if (= 1 1) 0 5)
; 0
;
; Delighted, Alyssa uses new-if to rewrite the square-root program:
;
; (define (sqrt-iter guess x)
;   (new-if (good-enough? guess x)
;           guess
;           (sqrt-iter (improve guess x)
;                      x)))
;
; What happens when Alyssa attempts to use this to compute square roots?
; Explain.

; The new-if function evaluates in applicative order. Thus, the else
; condition will be continuously evaluated, switching back and forth between
; calls to sqrt-iter and new_if. This is an endless loop.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.7.  The good-enough? test used in computing square roots will not be
; very effective for finding the square roots of very small numbers. Also, in
; real computers, arithmetic operations are almost always performed with
; limited precision. This makes our test inadequate for very large numbers.
; Explain these statements, with examples showing how the test fails for small
; and large numbers. An alternative strategy for implementing good-enough? is
; to watch how guess changes from one iteration to the next and to stop when the
; change is a very small fraction of the guess. Design a square-root procedure
; that uses this kind of end test. Does this work better for small and large
; numbers?

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; The good enough approximation is limited by 0.001 which is not helpful for
; numbers with much less significance than that. See the following example.
(sqrt 0.00000000004)

; Likewise, it is also not good for numbers of large magnitude as the value
; of the square root can inherently make the impact of approximation within
; the 0.001 bound much more drastic. See the example below.
(sqrt 40022525)


(define (good-enough? guess x)
	(< (abs (- guess (improve guess x))) 0.00000000001))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.8.  Newton's method for cube roots is based on the fact that if
; y is an approximation to the cube root of x, then a better approximation is
; given by the value. Use this formula to implement a cube-root procedure
; analogous to the square-root procedure.

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (good-enough? guess x)
  (< (abs (- (* guess guess guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 27)
(cbrt 81)
(cbrt 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.9.  Each of the following two procedures defines a method for
; adding two positive integers in terms of the procedures inc, which
; increments its argument by 1, and dec, which decrements its argument by 1.

; (define (+ a b)
;   (if (= a 0)
;       b
;       (inc (+ (dec a) b))))
;
; (define (+ a b)
;   (if (= a 0)
;       b
;       (+ (dec a) (inc b))))

; Using the substitution model, illustrate the process generated by each
; procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

; First definition:
; (+ 4 5)
; (inc (+ (dec 4) 5))
; (inc (+ 3 5))
; (inc (inc (+ (dec 3) 5)))
; (inc (inc (+ 2 5)))
; (inc (inc (inc (+ (dec 2) 5))))
; (inc (inc (inc (+ 1 5))))
; (inc (inc (inc (inc (+ (dec 1) 5)))))
; (inc (inc (inc (inc (+ 0 5)))))
; (inc (inc (inc (inc 5))))
; (inc (inc (inc 6)))
; (inc (inc 7))
; (inc 8)
; 9
; This process is recursive as the function is nested within a different
; funciton in the output.

; Second definition:
; (+ 4 5)
; (+ (dec 4) (inc 5))
; (+ 3 6)
; (+ (dec 3) (inc 6))
; (+ 2 7)
; (+ (dec 2) (inc 7))
; (+ 1 8)
; (+ (dec 1) (inc 8))
; (+ 0 9)
; 9
; This process is iterative. A single call of the larger function, not nested
; within itself is called in the output.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.10.  The following procedure computes a mathematical function
; called Ackermann's function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; What are the values of the following expressions?

(A 1 10)
; (A 0 (A 1 9))
; (* 2 (A 1 9))
; (* 2 (* 2 (A 1 8)))
; ...
; 1024

(A 2 4)
; (A 1 (A 2 3))
; (A 1 (A 1 (A 2 2)))
; (A 1 (A 1 (A 1 (A 2 1))))
; (A 1 (A 1 (A 1 2)))
; (A 1 (A 1 (A 0 (A 1 1))))
; (A 1 (A 1 (A 0 2)))
; (A 1 (A 1 4))
; (A 1 (A 0 (A 1 3)))
; (A 1 (A 0 (A 0 (A 1 2))))
; (A 1 (A 0 (A 0 (A 0 (A 1 1)))))
; (A 1 (A 0 (A 0 (A 0 2))))
; ...
; (A 1 16)
; (A 0 (A 1 15))
; ...
; 65536


(A 3 3) ;
; (A 2 (A 3 2))
; (A 2 (A 2 (A 3 1)))
; (A 2 (A 2 2))
; (A 2 (A 1 (A 2 1)))
; (A 2 (A 1 2))
; (A 2 (A 0 (A 1 1)))
; (A 2 (A 0 2))
; (A 2 4)
; ...
; 65536

; Consider the following procedures, where A is the procedure defined above:


(define (f n) (A 0 n))
; 2n

(define (g n) (A 1 n))
; 2^n

(define (h n) (A 2 n))
; 2^2^2^...

(define (k n) (* 5 n n))

; Give concise mathematical definitions for the functions computed by the
; procedures f, g, and h for positive integer values of n. For example, (k n)
; computes 5n2.

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.11.  A function f is defined by the rule that f(n) = n if n<3 and
; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n>=3. Write a procedure that
; computes f by means of a recursive process. Write a procedure that computes
; f by means of an iterative process.

; recusive
(define (f n)
    if (n < 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))

; iterative
(define (f n)
  (define f-iter a b c count)
    if (count < 3)
      a
      (f_iter (+ a (* 2 b) (*3 c)) a b (- count 1))
  if (n < 3)
    n
    (f-iter 2 1 0 n))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.12.  The following pattern of numbers is called Pascal's triangle.


; The numbers at the edge of the triangle are all 1, and each number inside the
; triangle is the sum of the two numbers above it. Write a procedure that
; computes elements of Pascal's triangle by means of a recursive process.

(define (pascal row col)
  if (or (col = 1) (col = row))
    1
    (+ (pascal (- row 1) (- col 1)) (pascal (- row 1) col)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 1.15.  The sine of an angle (specified in radians) can be computed by
; making use of the approximation sin x ~ x if x is sufficiently small, and the
; trigonometric identity sin x = 3 sin x/3 - 4 sin^3 x/3
; to reduce the size of the argument of sin. (For purposes of this exercise an
; angle is considered ``sufficiently small'' if its magnitude is not greater
; than 0.1 radians.) These ideas are incorporated in the following procedures:

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))

; a.  How many times is the procedure p applied when (sine 12.15) is evaluated?
; (sine 12.15)
; (p (sine (/ 12.15 3.0)))
; (p (sine (4.05)))
; (p (p (sine (/ 4.05 3.0))))
; (p (p (sine 1.35)))
; (p (p (p (sine (/ 1.35 3.0)))))
; (p (p (p (sine 0.45))))
; (p (p (p (p (sine (/ 0.45 3.0))))))
; (p (p (p (p (sine 0.15)))))
; (p (p (p (p (p (sine (/ 0.15 3.0)))))))
; (p (p (p (p (p (sine 0.05))))))
; (p (p (p (p (p 0.05)))))
; p is evaluated five times

; b.  What is the order of growth in space and number of steps (as a function
; of a) used by the process generated by the sine procedure when (sine a)
; is evaluated?
; sine divides by 3 each time making the process of finding an angle value that
; will end the recursion of sine a logarithm process. This is because we are
; finding final_angle < 0.1 such that final_angle = angle/3^n where n is the
; number of steps that we are iterating through.
