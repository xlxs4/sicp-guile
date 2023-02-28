;;; Exercise 1.30
;; The sum procedure above generates a linear
;; recursion. The procedure can be rewritten
;; so that the sum is performed iteratively.
;; Show how to do this by filling in the
;; missing expressions in the following definition:

;; (define (sum term a next b)
;;   (define (iter a result)
;;     (if <??>
;;         <??>
;;         (iter <??> <??>)))
;;   (iter <??> <??>))

(define (pi n)
  (define (pi-sum a b)
    (define (term x)
      (/ 1.0 (* x (+ x 2))))
    (define (next x)
      (+ x 4))
    (sum term a next b))
  (* 8 (pi-sum 1 n)))

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(pi 10000000)
