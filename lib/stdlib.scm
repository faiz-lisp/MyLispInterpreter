;; ====================
;; High-order utilities
;; ====================

(define (compose f g)
  (lambda x (f (apply g x))))

;; ==============
;; List functions
;; ==============

(define (null? lst) (equal? lst '()))

(define (list . lst) lst)

(define (caar pair)
  (car (car pair)))

(define (cadr pair)
  (car (cdr pair)))

(define (foldl f ret lst)
  (if (null? lst)
      ret
      (foldl f (f ret (car lst)) (cdr lst))))

(define (foldr f ret lst)
  (if (null? lst)
      ret
      (f (car lst) (foldr f ret (cdr lst)))))

(define (length lst) (foldl (lambda (x y) (+ x 1)) 0 lst))

(define (sum lst) (apply + lst))

(define (product lst) (foldl * 1 lst))



(define (not b) (if b #f #t))

(define (boolean? obj)
  (or (equal? obj #t) (equal? obj #f)))

(define (zero? z) (= z 0))

(define (positive? x) (> x 0))

(define (negative? x) (< x 0))

(define (odd? n) (= (remainder n 2) 1))

(define (even? n) (not (odd? n)))

(define (succ n) (+ n 1))

(define (id x) x)

(define (negate x) (- x))

(define (abs x) (if (negative? x) (negate x) x))

(define (range a1 an)
  (if (> a1 an)
      '()
      (cons a1 (range (succ a1) an))))

(define (fact n)
  (product (range 1 n)))

(define (gcd . lst)
  (define (iter a b)
    (if (= b 0)
        a
        (iter b (modulo a b))))
  (abs (cond ((null? lst) 0)
             ((null? (cdr lst)) (car lst))
             (else (foldl iter (car lst) (cdr lst))))))

(fact 1000)