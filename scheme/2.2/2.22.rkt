;;; Exercise 2.22
;; Louis Reasoner tries to rewrite the first
;; square-list procedure of Exercises 2.21 so that
;; it evolves an iterative process:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons (square (car things))
;;                     answer))))
;;   (iter items nil))

;; Unfortunately, defining square-list this way
;; produces the answer list in the reverse order
;; of the one desired. Why?

;; Louis then tries to fix his bug by interchanging
;; the arguments to cons:

;; (define (square-list items)
;;   (define (iter things answer)
;;     (if (null? things)
;;         answer
;;         (iter (cdr things)
;;               (cons answer
;;                     (square
;;                      (car things))))))
;;   (iter items nil))

;; This doesn't work either. Explain.

;;; Answer
;; The first answer gives us the elements in reverse:
;; > (square-list '(1 2 3 4 5))
;; '(25 16 9 4 1)

;; The second answer yields this:
;; > (square-list '(1 2 3 4 5))
;; '(((((() . 1) . 4) . 9) . 16) . 25)

;; The first one is pretty self-explanatory.
;; cons x y will create a cons cell (pair) with the
;; first value as the left element (car) and the second
;; as the right (cdr).

;; To construct a linked list using cons cells that looked
;; liked '(1 4 9 16 25), we'd need to first create the cons
;; cell (25 . nil). Then, we'de create a new cons cell with
;; (16 . (25 . nil)) and so one, until we arrive at
;; (1 . (4 . (9 . (16 . (25 . nil))))), which is shown as
;; (1 4 9 16 25).
;; But, when starting with the empty list, the first cons
;; creates the pair '(() . (1 . ())), which has the empty
;; list as its car and (1 . nil) as its cdr.
