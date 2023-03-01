;;; Exercise 2.6
;; In case representing pairs as procedures
;; wasn't mind-boggling enough, consider that,
;; in a language that can manipulate procedures,
;; we can get by without numbers (at least insofar
;; as nonnegative integers are concerned) by
;; implementing 0 and the operation of adding 1 as

;; (define zero (lambda (f) (lambda (x) x)))

;; (define (add-1 n)
;;   (lambda (f) (lambda (x) (f ((n f) x)))))

;; This representation is known as *Church numerals*,
;; after its inventor, Alonzo Church, the logician
;; who invented the λ-calculus.

;; Define one and two directly (not in terms of zero
;; and add-1). (Hint: Use substitution to evaluate
;; (add-1 zero)). Give a direct definition of the
;; addition procedure + (not in terms of repeated
;; application of add-1).

;;; Answer:
;; Let's evaluate (add-1 zero) first, using
;; applicative-order evaluation:

;; (add-1 zero)
;; (add-1 (lambda (f) (lambda (x) x)))
;; ((lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))))
;; ((lambda (f) (lambda (x) (f ((lambda (x) x) x)))))
;; (lambda (f) (lambda (x) (f x)))

;; So we got our answer!
(define one
  (lambda (f) (lambda (x) (f x))))

(define two
  (lambda (f) (lambda (x) (f (f x)))))

;; three would be
;; (lambda (f) (lambda (x) (f (f (f x)))))
;; and so on. That's why add-1 is defined as
;; (lambda(f) (lambda (x) (f ((n f) x)))).

;; Now, we could just "derive" the addition procedure.
;; In fact, I haven't come across one single person online
;; that actually bothers to explain their thinking behind
;; this, people just conjure it out of thin air; not cool.
;; This exercise is one of the reasons why I love SICP.
;; Iff you decide to look into this, you'll dip your toes
;; in the lambda sea, a great CS subject with a lot of value.
;; But, it's up to you. They're not going to hold your hand
;; and walk you through it. BTW, you can also pick up "The
;; Little Schemer". You'll end up implementing a Y combinator
;; if you follow that book ;)

;; So, let's briefly talk about lambda calculus in order
;; for this exercise to make sense. Lambda calculus is a
;; formalism, a system to investigate the foundations of
;; mathematics. Lambda is the name of the greek symbol λ.
;; Why did Church base all of this on this symbol? We don't
;; know, because Church himself has given different,
;; conflicting answers about this.

;; It's a very elegant model of computation. It is a
;; universal model of computation, meaning it is Turing-complete.
;; Meaning you can do everything you do using a programming
;; language using nothing but lambda calculus. The elegance,
;; to me at least, is due to the fact that lambda calculus is
;; a set of very very few, very simple rules.

;; Lambda calculus is only about functions and variables.
;; Functions in lambda calculus bind only one variable.
;; So f(x) is allowed, but f(x, y) isn't. Also, all functions
;; in lambda calculus are pure. This means that they only
;; return a value, there's no side-effects. Your typical
;; print-esque procedure is an impure function. It returns
;; nothing, yet it prints things to a stream.

;; In lambda calculus, (lambda (x) x) would be λx.x
;; Conversely, (lambda (x) (lambda (y) (+ x y))) would be
;; λx.λy.x+y
;; Fun fact: notice that f(x, y) : x + y is equivalent to
;; f(x) : f(y) : x + y. That's why you don't need multi-
;; variable functions. That technique is called currying,
;; and it's pretty useful in programming languages that
;; support this mechanism, since it can be used for
;; partially applying a function: Doing something, and
;; returning a function that does the rest, deferring
;; computation at a later stage.

;; Onto something more practical. We can think of
;; nonnegative integers as a way to count. How can we
;; count using only functions? Well, yes. We can count
;; function applications. That's why zero was defined as
;; (lambda (f) (lambda (x) x))
;; and that's why we ended up defining one as
;; (lambda (f) (lambda (x) (f x))))
;; and two as
;; (lambda (f) (lambda (x) (f (f x)))))

;; And that's why add-1 is
;; (lambda (f) (lambda (x) (f ((n f) x)))))
;; It makes perfect sense. We take a numeral n, which is
;; conveyed by n applications of f, and then we apply f
;; one more time. add-2 would be
;; (lambda (f) (lambda (x) (f (f ((n f) x)))))
;; and so on.

;; For the addition procedure, we want to somehow extend
;; add-1. But instead of applying f one more time, we
;; would want to apply f m more times.
;; (lambda (f) (lambda (x) ((m f) ((n f) x))))

(define (add m n)
  (lambda (f) (lambda (x) ((m f) ((n f) x)))))

;; There's an easy way to check what we did is right:

(define (inc x) (+ x 1))

(define (church->int c)
  ((c inc) 0))

;; > (church->int (add one two))
;; 3

;; You can do more... a lot more. Multiplication,
;; booleans, logic operators...
