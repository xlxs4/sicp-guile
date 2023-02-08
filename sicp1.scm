; Utilities
(define (square x) (* x x))

#| Exercise 1.1
Below is a sequence of expressions. What is the result printed by
the interpreter in response to each expression? Assume that the sequence
is to be evaluated in the order in which it is presented.
|#

#| Answer:
10
scheme@(guile-user)> 10

(+ 5 3 4)
scheme@(guile-user)> 12

(- 9 1)
scheme@(guile-user)> 8

(/ 6 2)
scheme@(guile-user)> 3

(+ (* 2 4) (- 4 6))
scheme@(guile-user)> 6

(define a 3)

(define b (+ a 1))

(+ a b (* a b))
scheme@(guile-user)> 19

(= a b)
scheme@(guile-user)> #f

(if (and (> b a) (< b (* a b)))
    b
    a)
scheme@(guile-user)> 4

(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
scheme@(guile-user)> 16

(+ 2 (if (> b a) b a))
scheme@(guile-user)> 6

(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))
scheme@(guile-user)> 16
|#

#| Exercise 1.2
Translate the following expression into prefix form:

5 + 4 + (2 - (3 - (6 + 4/5)))
-----------------------------
       3(6 - 2)(2 - 7)
|#

#| Answer:
(/ (+ (+ 5 4) (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))
|#

#| Exercise 1.3
Define a procedure that takes three numbers as arguments
and returns the sum of the squares of the two larger numbers.
|#

(define (largest-squares x y z)
  (cond
   ((and (>= x z) (>= y z)) (+ (square x) (square y)))
   ((and (>= y x) (>= z x)) (+ (square y) (square z)))
   ((and (>= x y) (>= z y)) (+ (square x) (square z)))))

#| Exercise 1.4
Observe that our model of evaluation allows for combinations
whose operators are compound expressions. Use this observation
to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
|#

#| Answer:
The if statement returns either `-` or `+`,
which is then applied to the operands:
If b is greater than 0, then a + b.
Else, a - b.
|#

#| Exercise 1.5
Ben Bitdiddle has invented a test to determine whether
the interpreter he is faced with is using
applicative-order evaluation or normal-order evaluation.
He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

Then he evaluates the expression

(test 0 (p))

What behavior will Ben observe with the interpreter
that uses applicative-order evaluation? What behavior
will he observe with an interpreter that uses normal-order evaluation?
Explain your answer. (Assume that the evaluation rule for the
special form `if` is the same whether the interpreter is using
normal or applicative order: The predicate expression is evaluated first,
and the result determines whether to evaluate
the consequent or the alternative expression.)
|#

#| Answer:
If the interpreter uses applicative-order evaluation,
the argument subexpressions are evaluated first.
These are `0` and `(p)`. `0` evaluates to the value `0`.
`(p)` is a call to the procedure `p`. Following on with the evaluation order,
we can think of `p` being "replaced" with the procedure body.
The procedure body is, again, `(p)`, which means evaluating the procedure
requires evaluating the procedure, and we're stuck
in what can be thought of as an "evaluation loop".
While the procedure is an argument that we know will
not be used when evaluating the special form `if`,
it still is responsible for non-terminating evaluation.

This wouldn't happen with normal-order evaluation.
We can think that the call is replaced with the body (the `if` form)
and then that the formal arguments `x` and `y` are
replaced with `0` and `(p)`. We end up at `(if (= 0 0) 0 (p))`.
The predicate expression is evaluated first, which means the
consequent, `0`, is evaluated next. We get the value of `0`
and evaluation stops there.

This isn't the whole story — there's still environments and renaming
to think about, but that's a good enough explanation for now.
|#

#| Exercise 1.6
Alyssa P. Hacker doesn't see why `if` needs to be
provided as a special form. "Why can't I just define it
as an ordinary procedure in terms of `cond`?" she asks.
Alyssa's friend Eva Lu Ator claims that this can indeed be done,
and she defines a new version of `if`:

(define (new-if predicate
                then-clause
                else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

Eva demonstrates the program for Alyssa:

(new-if (= 2 3) 0 5)
5

(new-if (= 1 1) 0 5)
0

Delighted, Alyssa uses `new-if` to rewrite the square-root program:

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))

What happens when Alyssa attempts to use this to compute
square roots? Explain.
|#

#| Answer:
Scheme is an applicative-order language. As we saw in Exercise 1.5,
if `new-if` is an ordinary procedure instead of a special form,
that means that the predicate, the consequent, and the alternative
will all be evaluated before evaluating the procedure itself.
We can see that `sqrt-iter` is recursively defined —
calling `new-if` would mean we need to evaluate `sqrt-iter` while
we're still evaluating `sqrt-iter`, which results in an evaluation loop.
|#
