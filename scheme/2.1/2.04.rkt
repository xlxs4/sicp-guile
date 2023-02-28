;;; Exercise 2.4
;; Here is an alternative procedural representation
;; of pairs. For this representation, verify that
;; (car (cons x y)) yields x for any objects x and y.

;; (define (cons x y)
;;   (lambda (m) (m x y)))

;; (define (car z)
;;   (z (lambda (p q) p)))

;; What is the corresponding definition of cdr?
;; (Hint: To verify that this works, make use of the
;; substitution model of 1.1.5.)

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;; Let's use our good-ol' applicative-order evaluation:
;; (car (cons x y))
;; (car (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) p))
;; ((lambda (p q) p) x y)
;; x

;; Similarly:
;; (cdr (cons x y))
;; (cdr (lambda (m) (m x y)))
;; ((lambda (m) (m x y)) (lambda (p q) q))
;; ((lambda (p q) q) x y)
;; y
