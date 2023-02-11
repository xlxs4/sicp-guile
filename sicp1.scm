;;; Utilities
(define (square x) (* x x))
(define (average x y) (/ (+ x y) 2))

(define (inc x) (+ x 1))
(define (dec x) (- x 1))

;;; Exercise 1.1
;; Below is a sequence of expressions. What is the result printed by
;; the interpreter in response to each expression? Assume that the sequence
;; is to be evaluated in the order in which it is presented.

;;; Answer:
;; 10
;; scheme@(guile-user)> 10

;; (+ 5 3 4)
;; scheme@(guile-user)> 12

;; (- 9 1)
;; scheme@(guile-user)> 8

;; (/ 6 2)
;; scheme@(guile-user)> 3

;; (+ (* 2 4) (- 4 6))
;; scheme@(guile-user)> 6

;; (define a 3)

;; (define b (+ a 1))

;; (+ a b (* a b))
;; scheme@(guile-user)> 19

;; (= a b)
;; scheme@(guile-user)> #f

;; (if (and (> b a) (< b (* a b)))
;;     b
;;     a)
;; scheme@(guile-user)> 4

;; (cond ((= a 4) 6)
;;       ((= b 4) (+ 6 7 a))
;;       (else 25))
;; scheme@(guile-user)> 16

;; (+ 2 (if (> b a) b a))
;; scheme@(guile-user)> 6

;; (* (cond ((> a b) a)
;;          ((< a b) b)
;;          (else -1))
;;    (+ a 1))
;; scheme@(guile-user)> 16

;;; Exercise 1.2
;; Translate the following expression into prefix form:

;; 5 + 4 + (2 - (3 - (6 + 4/5)))
;; -----------------------------
;;        3(6 - 2)(2 - 7)

;;; Answer:
;; (/ (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))

;;; Exercise 1.3
;; Define a procedure that takes three numbers as arguments
;; and returns the sum of the squares of the two larger numbers.

(define (largest-squares x y z)
  (define (sum-squares a b) (+ (square a) (square b)))

  (cond ((and (<= x y) (<= x z)) (sum-squares y z))
        ((and (<= y x) (<= y z)) (sum-squares x z))
        (else (sum-squares x y))))

;;; Exercise 1.4
;; Observe that our model of evaluation allows for combinations
;; whose operators are compound expressions. Use this observation
;; to describe the behavior of the following procedure:

;; (define (a-plus-abs-b a b)
;;   ((if (> b 0) + -) a b))

;;; Answer:
;; The if statement returns either - or +,
;; which is then applied to the operands:
;; If b is greater than 0, then a + b.
;; Else, a - b.

;;; Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether
;; the interpreter he is faced with is using
;; applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

;; (define (p) (p))

;; (define (test x y)
;;   (if (= x 0)
;;       0
;;       y))

;; Then he evaluates the expression

;; (test 0 (p))

;; What behavior will Ben observe with the interpreter
;; that uses applicative-order evaluation? What behavior
;; will he observe with an interpreter that uses normal-order evaluation?
;; Explain your answer. (Assume that the evaluation rule for the
;; special form if is the same whether the interpreter is using
;; normal or applicative order: The predicate expression is evaluated first,
;; and the result determines whether to evaluate
;; the consequent or the alternative expression.)

;;; Answer:
;; If the interpreter uses applicative-order evaluation,
;; the argument subexpressions are evaluated first.
;; These are 0 and (p). 0 evaluates to the value 0.
;; (p) is a call to the procedure p. Following on with the evaluation order,
;; we can think of p being "replaced" with the procedure body.
;; The procedure body is, again, (p), which means evaluating the procedure
;; requires evaluating the procedure, and we're stuck
;; in what can be thought of as an "evaluation loop".
;; While the procedure is an argument that we know will
;; not be used when evaluating the special form if,
;; it still is responsible for non-terminating evaluation.

;; This wouldn't happen with normal-order evaluation.
;; We can think that the call is replaced with the body (the if form)
;; and then that the formal arguments x and y are
;; replaced with 0 and (p). We end up at (if (= 0 0) 0 (p)).
;; The predicate expression is evaluated first, which means the
;; consequent, 0, is evaluated next. We get the value of 0
;; and evaluation stops there.

;; This isn't the whole story — there's still environments and renaming
;; to think about, but that's a good enough explanation for now.

;;; Exercise 1.6
;; Alyssa P. Hacker doesn't see why if needs to be
;; provided as a special form. "Why can't I just define it
;; as an ordinary procedure in terms of cond?" she asks.
;; Alyssa's friend Eva Lu Ator claims that this can indeed be done,
;; and she defines a new version of if:

;; (define (new-if predicate
;;                 then-clause
;;                 else-clause)
;;   (cond (predicate then-clause)
;;         (else else-clause)))

;; Eva demonstrates the program for Alyssa:

;; (new-if (= 2 3) 0 5)
;; 5

;; (new-if (= 1 1) 0 5)
;; 0

;; Delighted, Alyssa uses new-if to rewrite the square-root program:

;; (define (sqrt-iter guess x)
;;   (new-if (good-enough? guess x)
;;           guess
;;           (sqrt-iter (improve guess x) x)))

;; What happens when Alyssa attempts to use this to compute
;; square roots? Explain.

;;; Answer:
;; Scheme is an applicative-order language. As we saw in Exercise 1.5,
;; if new-if is an ordinary procedure instead of a special form,
;; that means that the predicate, the consequent, and the alternative
;; will all be evaluated before evaluating the procedure itself.
;; We can see that sqrt-iter is recursively defined —
;; calling new-if would mean we need to evaluate sqrt-iter while
;; we're still evaluating sqrt-iter, which results in an evaluation loop.

;;; Exercise 1.7
;; The good-enough? test used in computing square roots will not
;; be very effective for finding the square roots of very small numbers.
;; Also, in real computers, arithmetic operations are almost always
;; performed with limited precision. This makes our test inadequate
;; for very large numbers. Explain these statements, with examples
;; showing how the test fails for small and large numbers.
;; An alternative strategy for implementing good-enough?
;; is to watch how guess changes from one iteration to the next
;; and to stop when the change is a very small fraction of the guess.
;; Design a square-root procedure that uses this kind of end test.
;; Does this work better for small and large numbers?

;;; Answer:
;; Let's look into small numbers first.

;; scheme@(guile-user)> (my-sqrt 1.0e-8)
;; 0.03125010656242753

;; scheme@(guile-user)> (square (my-sqrt 1.0e-8))
;; $9 = 9.76569160163076e-4

;; scheme@(guile-user)> (use-modules (system vm trace))
;; scheme@(guile-user)> ,trace (my-sqrt 1.0e-8)
;; trace: |  (my-sqrt 1.0e-8)
;; trace: |  |  (square 1.0)
;; trace: |  |  1.0
;; trace: |  |  (average 1.0 1.0e-8)
;; trace: |  |  0.500000005
;; trace: |  |  (square 0.500000005)
;; trace: |  |  0.25000000499999997
;; trace: |  |  (average 0.500000005 1.9999999800000004e-8)
;; trace: |  |  0.25000001249999987
;; trace: |  |  (square 0.25000001249999987)
;; trace: |  |  0.06250000625000009
;; trace: |  |  (average 0.25000001249999987 3.999999800000012e-8)
;; trace: |  |  0.12500002624999892
;; trace: |  |  (square 0.12500002624999892)
;; trace: |  |  0.01562500656250042
;; trace: |  |  (average 0.12500002624999892 7.999998320000422e-8)
;; trace: |  |  0.06250005312499106
;; trace: |  |  (square 0.06250005312499106)
;; trace: |  |  0.003906256640626705
;; trace: |  |  (average 0.06250005312499106 1.599998640001385e-7)
;; trace: |  |  0.03125010656242753
;; trace: |  |  (square 0.03125010656242753)
;; trace: |  |  9.76569160163076e-4
;; trace: |  0.03125010656242753

;; Since good-enough? uses a tolerance of 0.001 it follows that
;; our sqrt procedure cannot provide a result
;; smaller than about the tolerance, which renders the procedure
;; ineffective when smaller numbers are involved.
;; It's like trying to approximate the length of a needle
;; with an error margin of plus/minus 1 meter.

;; What about big numbers?

;; scheme@(guile-user)> (my-sqrt 1.0e13)
;; <evaluation doesn't terminate>

;; scheme@(guile-user)> ,trace (my-sqrt 1.0e13)
;; trace: |  (my-sqrt 1.0e13)
;; trace: |  |  (square 1.0)
;; trace: |  |  1.0
;; trace: |  |  (average 1.0 1.0e13)
;; trace: |  |  5000000000000.5
;; trace: |  |  (square 5000000000000.5)
;; trace: |  |  2.5000000000005e25
;; trace: |  |  (average 5000000000000.5 1.9999999999998)
;; trace: |  |  2500000000001.25
;; trace: |  |  (square 2500000000001.25)
;; trace: |  |  6.25000000000625e24
;; <after some more iterations...>
;; trace: |  |  (square 3162277.6640104805)
;; trace: |  |  10000000024299.582
;; trace: |  |  (average 3162277.6640104805 3162277.656326278)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002
;; trace: |  |  (average 3162277.6601683795 3162277.660168379)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002
;; trace: |  |  (average 3162277.6601683795 3162277.660168379)
;; trace: |  |  3162277.6601683795
;; trace: |  |  (square 3162277.6601683795)
;; trace: |  |  10000000000000.002

;; "Squeezing infinitely many real numbers into a finite
;; number of bits requires an approximate representation. [...]"

;; — Goldberg, D. (1991) What every computer scientist should know
;; about floating-point arithmetic. ACM computing surveys (CSUR), 23(1), 5-48.

;; Our improve procedure can't improve the guess due to rounding errors
;; that are inherent to floating-point arithmetic. The smallest possible difference
;; between (square guess) and x is larger than our tolerance, 0.001.
;; In other words, due to how the number is encoded in memory,
;; the distance between two consequtive floats in this
;; order of magnitude is larger than 0.001.

;; An interesting observation to be made here is that eliminating these singularities
;; requires a little more involvement from our side than simply messing with the tolerance.
;; To be able to more accurately approximate the square root of small numbers,
;; we can decrease the tolerance, say to 1.0e-7. However, this will mean
;; we now cannot compute the square root of numbers as big as the ones we previously could,
;; due to the rounding errors introduced and how they compare relative to the tolerance.
;; Changing the tolerance doesn't improve our procedure — it shifts the window
;; in the number line in which we can effectively approximate square roots.

;; Let's improve the procedure as instructed by the exercise definition next:

(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- guess (improve guess))) 0.001))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; scheme@(guile-user)> (my-sqrt 1.0e13)
;; 3162277.6601683795

;; Fixed!

;; Essentially the only thing of interest is that
;; (- (square guess) x) has changed to
;; (- guess (average guess (/ x guess)))

;; This guarantees that evaluation will always terminate.

;; Of course the underlying precision issues are still present,
;; for example (compare with further below):

;; scheme@(guile-user)> (square (my-sqrt 1.0e-16))
;; 3.8146972656916666e-6

;; Observe that now we call improve twice for each iteration —
;; the second call is hidden inside good-enough?.
;; To fix this, we could assign the return value of improve to a variable
;; and reference it later to call it only once,
;; but we don't know how to do that yet.

;; Lastly, there's a clever simple trick.
;; "[...] in real computers, arithmetic operations are almost always
;; performed with limited precision." What does this mean?
;; It means that we can know for sure that at some point,
;; improve won't change the guess any more.
;; We can do a small modification in good-enough? to achieve
;; the maximum precision available. Instead of defining a tolerance,
;; we can check instead that the guess hasn't changed at all:

(define (my-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (= guess (improve guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;; scheme@(guile-user)> (square (my-sqrt 1.0e-16))
;; 1.0000000000000001e-16

;;; Exercise 1.8
;; Newton's method for cube roots is based on the fact that if y
;; is an approximation to the cube root of x, then
;; a better approximation is given by the value

;; x/y^2 + 2y
;; ----------
;;     3

;; Use this formula to implement a cube-root procedure
;; analogous to the square-root procedure.
;; (In 1.3.4 we will see how to implement Newton's method
;; in general as an abstraction of these square-root
;; and cube-root procedures).

(define (my-cube x)
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (good-enough? guess)
    (= guess (improve guess)))
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (iter 1.0))

;;; Exercise 1.9
;; Each of the following two procedures defines a method
;; for adding two positive integers in terms of the procedures
;; inc, which increments its argument by 1, and dec,
;; which decrements its argument by 1.

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

;; Using the substitution model, illustrate the process generated
;; by each procedure in evaluating (+ 4 5). Are these processes
;; iterative or recursive?

;;; Answer:
;; Let's take a look at the first version <trace simplified>:

;; scheme@(guile-user)> ,trace (+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  |  (+ 3 5)
;; trace: |  |  |  (+ 2 5)
;; trace: |  |  |  |  (+ 1 5)
;; trace: |  |  |  |  |  (+ 0 5)
;; trace: |  |  |  |  |  5
;; trace: |  |  |  |  6
;; trace: |  |  |  7
;; trace: |  |  8
;; trace: |  9

;; And the second version <trace simplified>:

;; scheme@(guile-user)> ,trace (+ 4 5)
;; trace: |  (+ 4 5)
;; trace: |  (+ 3 6)
;; trace: |  (+ 2 7)
;; trace: |  (+ 1 8)
;; trace: |  (+ 0 9)
;; trace: |  9

;; Both versions are a definition of a recursive procedure.
;; However, the first version generates a recursive process,
;; while the second version generates an iterative process.
;; Take a look at 1.2.1 "Linear Recursion and Iteration".

;;; Exercise 1.10
;; The following procedure computes a mathematical function called Ackermann's function.

;; (define (A x y)
;;   (cond ((= y 0) 0)
;;         ((= x 0) (* 2 y))
;;         ((= y 1) 2)
;;         (else (A (- x 1)
;;                  (A x (- y 1))))))

;; What are the values of the follwing expressions?

;; (A 1 10)
;; (A 2 4)
;; (A 3 3)

;; Consider the following procedures, where A is the procedure defined above:

;; (define (f n) (A 0 n))
;; (define (g n) (A 1 n))
;; (define (h n) (A 2 n))
;; (define (k n) (* 5 n n))

;; Give concise mathematical definitions for the functions
;; computed by the procedures f, g, and h for positive integer values of n.
;; For example, (k n) computes 5n^2.

;;; Answer:
;; scheme@(guile-user)> (A 1 10)
;; 1024

;; scheme@(guile-user)> (A 2 4)
;; 65536

;; scheme@(guile-user)> (A)
;; 65536

;; f(n) = 2n
;; g(n) = n^2
;; h(n) = 2↑n

