;;; Exercise 2.55
;; Eva Lu Ator types to the interpreter the expression

;; (car ''abracadabra)

;; To her surprise, the interpreter prints back quote.
;; Explain.

;;; Answer:
;; quote is a special form, where (quote a) is 'a.
;; In fact, the quotation mark is just a single-character
;; abbreviation for wrapping the next complete expression
;; with quote to form (quote <expression>). This is
;; important to preserve homoiconicity.

;; We can think of (car ''abracadabra) as
;; (car '(quote abracadabra)).
