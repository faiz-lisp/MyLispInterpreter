(define (null? lst) (eqv? lst '()))

(define (not x) (if x #f #t))

(define (id x) x)

(define (negate x) (- 0 x))

(define (list . objs) objs)

(define (xcons d a) (cons a d))

(define (list* . objs) (cons (cdr lst) (car lst)))

(define (curry func arg1) (lambda (arg) (apply func (cons arg1 (list arg)))))

(define (compose f g) (lambda (arg) (f (apply g arg))))

(define (fold f ret lst)
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

(define (filter pred . lst)
    (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() lst))

(define (?) (error "not implemented"))

(define (factorial n)
    (if (<= n 1)
        1
        (* n (factorial (- n 1)))))

(define (fib n)
    (if (< n 0)
        (error "'n' must be positive")
    (if (|| (= n 0) (= n 1))
        1
        (+ (fib (- n 1)) (fib (- n 2)))
    ))
)

(define (qsort . lst)
    (if (null? lst)
        '()
        (begin
            (define pivot (car lst))
            (define lst1 (qsort (filter (lambda (x) (<= x pivot)) lst)))
            (define lst2 (qsort (filter (lambda (x) (> x pivot)) lst)))
            (list lst1 pivot lst2))))

(begin
    (display "MyLisp Interpreter2")
    (newline)
    (newline)
    (display "Type (?) for a list of commands in standard library or (quit) to quit"))

(begin
    (display "MyLisp Interpreter")
    (newline)
    (newline)
    (display "Type (?) for a list of commands in standard library or (quit) to quit"))
