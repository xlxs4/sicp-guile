;;; Exercise 1.42
;; Let f and ge be two one-argument functions.
;; The composition f after g is defined to be
;; the function x ↦ f(g(x)). Defien a procedure
;; compose that implements composition. For example,
;; if inc is a procedure that adds 1 to its argument,

;; ((compose square inc) 6)
;; 49

(define (inc x) (+ x 1))
(define (square x) (* x x))

(define (compose f g)
  (lambda (x)
    (f (g x))))

;; scheme@(guile-user)> ((compose square inc) 6)
;; 49
