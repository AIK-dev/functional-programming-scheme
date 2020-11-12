(define top-left "\u250c")
(define top-right "\u2510")
(define bot-left "\u2514")
(define bot-right "\u2518")
(define horizontal "\u2500")
(define vertical "\u2502")

(define (id x) x)
(define (1+ x) (+ x 1))

(define (term-n n)
  (- (* 4 n) 3))

(define (prev-term-n n)
  (- (term-n n) 4))

(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
  (if (> a b) nv
      (accumulate-i op (op nv (term a)) (next a) b term next)))

(define (print-hat n)
  (begin
    (display top-left)
    (accumulate (lambda (x y) 
                  (begin
                   (display horizontal)))
                0 0 (- n 1) id 1+)
    (display top-right)))

(define (open-vertical n)
  (accumulate (lambda (x y)
                (begin
                  (display vertical)
                  (display " "))) 0 0 (- n 1) id 1+))
(define (open arg depth)
  (begin
    (newline)
    (open-vertical depth)
    (print-hat arg)
    (close-vertical depth)))
(define (close-vertical n)
  (accumulate (lambda (x y)
                (begin
                  (display " ")
                  (display vertical))) 0 0 (- n 1)
                                       (lambda (x) x) (lambda (x) (+ x 1))))

(define (close arg depth)
  (begin
    (newline)
    (open-vertical depth)
    (print-bottom arg)
    (close-vertical depth)))

(define (print-bottom n)
  (begin
    (display bot-left)
    (accumulate (lambda (x y)
                  (begin
                    (display horizontal)))
                0 0 (- n 1) id 1+)
    (display bot-right)))

(define (open-control arg counter)
  (accumulate-i (lambda (x y)
                  (begin ;First call should be with the same arg
                  (open (- arg (* (- y 1) 4)) y))) 0 1 counter id 1+))

(define (close-control arg counter)
  (accumulate (lambda (x y)
                (begin
                  (close (- arg (* (- x 1) 4)) x))) 0 1 counter id 1+))



(define (squares n)
  (begin
    (print-hat (term-n n))
    (open-control (prev-term-n n) (- n 1))
    (close-control (prev-term-n n) (- n 1))
    (newline)
    (print-bottom (term-n n))))

(squares 6)