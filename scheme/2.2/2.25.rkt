;;; Exercise 2.25
;; Give combinations of cars and cdrs that will
;; pick 7 from each of the following lists:

;; (1 3 (5 7) 9)
;; ((7))
;; (1 (2 (3 (4 (5 (6 7))))))

;;; Answer
;;

;; > (define xs1 '(1 3 (5 7) 9))
;; > (car (cdaddr xs1))
;; 7

;; > (define xs2 '((7)))
;; > (caar xs2)
;; 7

;; > (define xs3 '(1 (2 (3 (4 (5 (6 7)))))))
;; > (cadadr (cadadr (cadadr xs3)))
;; 7
