(define (null? lst) (eqv? lst '()))

(define (not x) (if x #f #t))

(define (id x) x)

(define (negate x) (- 0 x))

(define (list . objs) objs)

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g) (lambda (arg) (f (apply g arg))))

(define (foldl f ret lst)
    (if (null? lst)
        ret
        (foldl f (f ret (car lst)) (cdr lst))))

(define (foldr f ret lst)
    (if (null? lst)
        ret
        (f (car lst) (foldr f ret (cdr lst)))))

(define (sum . lst) (foldl + 0 lst))

(define (product . lst) (foldl * 1 lst))

(define (and . lst) (foldl && #t lst))

(define (or . lst) (foldl || #f lst))

(define (length lst) (foldl (lambda (x y) (+ x 1)) 0 lst))

(define (map f lst) (foldr (lambda (x y) (cons (f x) y)) '() lst))

(define (filter pred lst)
    (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (?) (error "not implemented"))

(begin
    (display "MyLisp Interpreter")
    (newline)
    (newline)
    (display "Type (?) for a list of commands in standard library or (quit) to quit"))
