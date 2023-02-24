;;; Exercise 1.31
;; 1. The sum procedure is only the simplest of a vast
;; number of similar abstractions that can be
;; captured as higher-order procedures. Write an
;; analogous procedure called product that returns
;; the product of the values of a function at points
;; over a given range. Show how to define factorial
;; in terms of product. Also use product to compute
;; approximations to π using the formula
;;
;;  π     2 · 4 · 4 · 6 · 6 · 8 · ...
;; --- = ----------------------------
;;  4     3 · 3 · 5 · 5 · 7 · 7 · ...

;; 2. If your product procedure generates a recursive
;; process, write one that generates an iterative process.
;; If it generates an iterative process, write one that
;; generates a recursive process.

(define (inc n) (+ n 1))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (pi n)
  (define (wallis a b)
    (define (term x)
      (if (even? x)
          (/ (+ x 2) (+ x 1))
          (/ (+ x 1) (+ x 2))))
    (product term a inc b))
  (* 4 (wallis 1.0 n)))

(pi 10000000)
