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

