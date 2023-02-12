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
;; (/ (+ (+ 5 4)
;;       (- 2 (- 3 (+ 6 (/ 4 5)))))
;;    (* 3
;;       (- 6 2)
;;       (- 2 7)))

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

;; What are the values of the following expressions?

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
;; h(n) = 2↑n <see Knuth's up-arrow notation>

;;; Exercise 1.11
;; A function f is defined by the rule that f(n) = n if n < 3
;; and f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n >= 3.
;; Write a procedure that computes f by means of a recursive process.
;; Write a procedure that computes f by means of an iterative process.

;;; Answer:
;; The recursive version is straightforward:

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

;; scheme@(guile-user)> ,trace (f 5)
;; trace: |  (f 5)
;; trace: |  |  (f 4)
;; trace: |  |  |  (f 3)
;; trace: |  |  |  |  (f 2)
;; trace: |  |  |  |  2
;; trace: |  |  |  |  (f 1)
;; trace: |  |  |  |  1
;; trace: |  |  |  |  (f 0)
;; trace: |  |  |  |  0
;; trace: |  |  |  4
;; trace: |  |  |  (f 2)
;; trace: |  |  |  2
;; trace: |  |  |  (f 1)
;; trace: |  |  |  1
;; trace: |  |  11
;; trace: |  |  (f 3)
;; trace: |  |  |  (f 2)
;; trace: |  |  |  2
;; trace: |  |  |  (f 1)
;; trace: |  |  |  1
;; trace: |  |  |  (f 0)
;; trace: |  |  |  0
;; trace: |  |  4
;; trace: |  |  (f 2)
;; trace: |  |  2
;; trace: |  25

;; The iterative version requires some extra thought.
;; We need 4 state variables — one so that our procedure terminates,
;; and 3 to capture f(n), f(n - 1), and f(n - 2), respectively.

(define (f n)
  (define (iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (iter
                 (+ a (* 2 b) (* 3 c))
                 a
                 b
                 (- count 1)))))
  (iter 2 1 0 (- n 2)))

;;; Exercise 1.12
;; The following pattern of numbers is called Pascal's triangle.

;;           1
;;         1   1
;;       1   2   1
;;    1    3    3   1
;; 1    4    6    4    1
;;         . . .

;; The numbers at the edge of the triangle are all 1, and each number
;; inside the triangle is the sum of the two numbers above it.
;; Write a procedure that computes elements of Pascal's triangle
;; by means of a recursive process.

(define (pascal row col)
  (if (or (= row col) (= col 1))
      1
      (+ (pascal (- row 1) (- col 1))
         (pascal (- row 1) col))))

;;; Exercise 1.13
;; Prove that Fib(n) is the closest integer to φ^n / sqrt(5), where
;; φ = (1 + sqrt(5)) / 2. Hint: Let ψ = (1 - sqrt(5)) / 2.
;; Use induction and the definition of the Fibonacci numbers (see 1.2.2)
;; to prove that Fib(n) = (φ^n - ψ^n) / sqrt(5).

;;; Answer:
;; Let's start from the hint. We need to prove that
;; Fib(n) = (φ^n - ψ^n) / sqrt(5). We also need to do that using induction,
;; therefore the base case is:
;; Fib(0) = (φ^0 - ψ^0) / sqrt(5) ^ Fib(1) = (φ - ψ) / sqrt(5)
;; we get that Fib(0) = 0 ^ Fib(1) = 1, which agrees
;; with the definition of the Fibonacci numbers.

;; Let's move on to the more interesting part of the proof by induction:
;; Let k be a natural number. If Fib(k) = (φ^k - ψ^k) / sqrt(5) and
;; Fib(k + 1) = (φ^(k + 1) - ψ^(k + 1)) / sqrt(5) are both true, then
;; it must hold that Fib(k + 2) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).
;; Using the definition of the Fibonacci numbers,
;; we can rewrite what we want to prove as:
;; Fib(k) + Fib(k + 1) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).

;; We can now use our hypotheses for Fib(k) and Fib(k + 1) to substitute:
;; ((φ^k - ψ^k) / sqrt(5)) + (φ^(k + 1) - ψ^(k + 1)) / sqrt(5) =
;; (φ^(k + 2) - ψ^(k + 2)) / sqrt(5). Simplifying:
;; (φ^k (φ + 1) - ψ^k (ψ + 1)) / sqrt(5) = (φ^(k + 2) - ψ^(k + 2)) / sqrt(5).
;; φ^k (φ + 1) - ψ^k (ψ + 1) = φ^(k + 2) - ψ^(k + 2).

;; We know that φ^2 = φ + 1, since φ is the only positive solution
;; to the equation x^2 = x + 1. ψ is similarly the only negative solution.
;; (See page 38.) Therefore, we get:
;; φ^k · φ^2 - ψ^k · ψ^2 = φ^(k + 2) - ψ^(k + 2).
;; φ^(k + 2) - ψ^(k + 2) = φ^(k + 2) - ψ^(k + 2)
;; So we have proven that Fib(n) = (φ^n - ψ^n) / sqrt(5).
;; Finally, we want to use this to prove the original statement —
;; that Fib(n) is the closest integer to φ^n / sqrt(5).

;; In other words, we want to prove that
;; | Fib(n) - φ^n / sqrt(5) | < 1/2.
;; We can use the statement that we have now proven to rewrite as:
;; | (φ^n - ψ^n) / sqrt(5) - φ^n / sqrt(5) | < 1/2. Simplifying:
;; | ψ^n | / sqrt(5) < 1/2
;; We can then substitute using the definition of ψ:
;; | ((1 - sqrt(5)) / 2)^n | /sqrt(5) < 1/2. Simplifying:
;; ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1/2.

;; We know that sqrt(9) = 3, so sqrt(5) < 3. Therefore:
;; We can subtract 1 to get sqrt(5) - 1 < 2,
;; divide by 2 to get (sqrt(5) - 1) / 2 < 1,
;; raise to n to get ((sqrt(5) - 1) / 2)^n < 1^n,
;; multiply by 1 / sqrt(5) to finally get
;; ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1 / sqrt(5).

;; We know that 1 / sqrt(5) < 1/2 since sqrt(5) > 2, if sqrt(4) = 2.
;; Therefore, ((sqrt(5) - 1) / 2)^n / sqrt(5) < 1/2.
;; QED.

;;; Exercise 1.14
;; Draw the tree illustrating the process generated by the count-change
;; procedure of 1.2.2 in making change for 11 cents.
;; What are the orders of growth of the space and number of steps
;; used by this process as the amount to be changed increases?

;;; Answer:
;; For a better tree look elsewhere ;)
;; scheme@(guile-user)> ,trace (count-change 11)
;; trace: |  (count-change 11)
;; trace: |  (cc 11 5)
;; trace: |  |  (cc 11 4)
;; trace: |  |  |  (cc 11 3)
;; trace: |  |  |  |  (cc 11 2)
;; trace: |  |  |  |  |  (cc 11 1)
;; trace: |  |  |  |  |  |  (cc 11 0)
;; trace: |  |  |  |  |  |  (cc 10 1)
;; trace: |  |  |  |  |  |  |  (cc 10 0)
;; trace: |  |  |  |  |  |  |  (cc 9 1)
;; trace: |  |  |  |  |  |  |  |  (cc 9 0)
;; trace: |  |  |  |  |  |  |  |  (cc 8 1)
;; trace: |  |  |  |  |  |  |  |  |  (cc 8 0)
;; trace: |  |  |  |  |  |  |  |  |  (cc 7 1)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 7 0)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 6 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 6 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 5 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 5 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 4 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 4 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 3 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 3 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 2 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 2 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  (cc 6 2)
;; trace: |  |  |  |  |  |  (cc 6 1)
;; trace: |  |  |  |  |  |  |  (cc 6 0)
;; trace: |  |  |  |  |  |  |  (cc 5 1)
;; trace: |  |  |  |  |  |  |  |  (cc 5 0)
;; trace: |  |  |  |  |  |  |  |  (cc 4 1)
;; trace: |  |  |  |  |  |  |  |  |  (cc 4 0)
;; trace: |  |  |  |  |  |  |  |  |  (cc 3 1)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 3 0)
;; trace: |  |  |  |  |  |  |  |  |  |  (cc 2 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 2 0)
;; trace: |  |  |  |  |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  (cc 1 2)
;; trace: |  |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  |  (cc -4 2)
;; trace: |  |  |  |  (cc 1 3)
;; trace: |  |  |  |  |  (cc 1 2)
;; trace: |  |  |  |  |  |  (cc 1 1)
;; trace: |  |  |  |  |  |  |  (cc 1 0)
;; trace: |  |  |  |  |  |  |  (cc 0 1)
;; trace: |  |  |  |  |  |  (cc -4 2)
;; trace: |  |  |  |  |  (cc -9 3)
;; trace: |  |  |  (cc -14 4)
;; trace: |  |  (cc -39 5)

;; On to the more interesting part of the exercise, the complexities.
;; Order of growth of space is straightforward, this is a recursive process,
;; so the order of growth of space will be proportional to the depth
;; of the procedure calls. And that order is Θ(n). The tree depth
;; grows linearly with n in (cc n m).

;; The order of growth of number of steps is a bit more involved.
;; Let's take a look at only using pennies first, i.e. m = 1:
;; (cc n 1) = (+ (cc n 0) (cc (- n d) 1), where d is the coin denomination.
;; Let's denote the order of growth in steps as a function of m and n, F(m, n).
;; F(n, 1) = 1 + F(n - d, 1) = 2 + F(n - 2d, 1), about n/d, so it's O(n).

;; Let's now look at the more general case.
;; (cc n m) = (+ (cc n (- m 1))
;;               (cc (- n dm) m) where dm is the denomination for m kinds.
;; = (+
;;      (cc n (- m 1))
;;      (cc (- n dm) (- m 1))
;;      (cc (- n (* 2 dm)) m))
;; = (+
;;      (cc n (- m 1))
;;      (cc (- n dm) (- m 1))
;;      ...
;;      (cc (- n (* fm dm)) (- m 1))
;;      (cc (- n (* (+ fm 1) dm)) m)) where fm is Floor(n/dm).
;; I'll do d and f instead of dm and fm now, for brevity's sake.

;; TODO :P

