;;; Suppose we evaluate the expression
;; (list 1 (list 2 (list 3 4))).
;; Give the result printed by the interpreter,
;; the corresponding box-and-pointer structure,
;; and the interpretation of this as a tree
;; (as in Figure 2.6).

;;; Answer:
;; Interpreter
;; > (list 1 (list 2 (list 3 4)))
;;'(1 (2 (3 4)))

;; Box
;;   (1 (2 (3 4)))    ((2 (3 4)))
;;         ↓               ↓
;;     [ · | · ]   →   [ · | / ]
;;       ↓               ↓               (3 4)
;;       1 (2 (3 4)) → [ · | · ]   →   [ · | / ]
;;                       ↓               ↓
;;                       2             [ · | · ]   →   [ · | / ]
;;                                       ↓               ↓
;;                                       3               4

;; Tree
;; (1 (2 (3 4)))
;;      ^
;;    /   \
;;   1     ^ (2 (3 4))
;;       /   \
;;      2     ^ (3 4)
;;          /   \
;;         3     4
