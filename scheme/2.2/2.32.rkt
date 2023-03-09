;;; Exercise 2.32
;; We can represent a set as a list of distinct
;; elements, and we can represent the set of all
;; subsets of the set as a list of lists.
;; For example, if the set is (1 2 3), then the
;; set of all subsets is
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).
;; Complete the following definition of a procedure
;; that generates the set of subsets of a set and
;; give a clear explanation of why it works:

;; (define (subsets s)
;;   (if (null? s)
;;       (list nil)
;;       (let ((rest (subsets (cdr s))))
;;         (append rest (map <??> rest)))))

;;; Answer:
;; The powerset of a set can be defined recursively.
;; If the set S is the empty set, then the powerset
;; of S, P(S), is also the empty set.
;; If the set S is not empty, then we can see that its
;; powerset consists of the union of two sets:
;; i) The powerset of the set that has as its elements
;; all of the elements of S except its first element.
;; ii) The powerset of the set that has as its elements
;; all of the elements of S except its first element,
;; *with the first element of S appended to all of
;; the subsets*.
;;
;; If S is '(), P(S) is '()
;; If S is '(1), P(S) is '(() (1))
;; If S is '(1 2), P(S) is '(() (1) (2) (1 2))
;;
;; Note that what we're doing here is very similar to
;; the logic used for counting change!

(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))
