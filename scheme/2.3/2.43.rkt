;;; Exercise 2.43
;; Louis Reasoner is having a terrible time doing
;; Exercise 2.42. His queens procedure seems to work,
;; but it runs extremely slowly. (Louis never does
;; manage to wait long enough for it to solve even
;; the 6 ✕ 6 case.) When Louis asks Eva Lu Ator for help,
;; she points out that he has interchanged the order
;; of the nested mappings in the flatmap, writing it as

;; (flatmap
;;  (lambda (new-row)
;;    (map (lambda (rest-of-queens)
;;           (adjoin-position
;;            new-row k rest-of-queens))
;;         (queen-cols (- k 1))))
;;  (enumerate-interval 1 board-size))

;; Explain why this interchange makes the program run
;; slowly. Estimate how long it will take Louis's
;; program to solve the eight-queens puzzle, assuming
;; that the program in Exercise 2.42 solves the puzzle
;; in time T.

;;; Answer:
;; The default implementation in Exercise 2.42 recursively
;; calls queen-cols for one column less. Then, all possible
;; new positions are appended and filtered to only keep those
;; that are "safe".

;; Interchanging the order of the nested mappings in the
;; flatmap, means that for each possible new positions,
;; queen-cols is called (again, for one column less),
;; and every new position is appended etc.
;; How would we avoid what Louis did? Is it up to chance?
;; No. What Louis has done doesn't really make sense and
;; there's an evident code smell to hint at that:
;; queen-cols doesn't do anything with new-row.

;; What about the math? The original queen-cols is called
;; once per execution (inside the lambda), for a total of
;; board-size + 1, or N + 1 times (although in the case of
;; N = 0, it returns empty-board immidiately.)

;; With what Louis has changed, now queen-cols is called
;; once per execution, but each call calls itself another
;; board-size times. There's N calls with N nested calls
;; each. What Louis has hacked together is O(N^N), which
;; is O(N^N) / N+1 = O(N^N) times slower, asymptotically.
