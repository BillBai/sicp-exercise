#lang scheme
(require (planet soegaard/sicp:2:1/sicp))

;1.3
(define (largest-two-sum x y z)
  (define (larger a b)
    (if (>= a b)
        a
        b))
  (+ (larger x y) (larger x z)))

;1.7
(define (sqrt x)
  (define (average m n)
  (/ (+ m n) 2.0))
  
  (define (good-enough? guess pre-guess)
    (define tolerance 0.0000001)
    (< (abs (- guess pre-guess)) tolerance))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (sqrt-iter guess pre-guess x)
    (if (good-enough? guess pre-guess)
        guess
        (sqrt-iter (improve guess) guess x)))
  
  (sqrt-iter 1.0 0.0 x))

;1.8
(define (cbrt x)
  (define (average m n)
  (/ (+ m n) 2.0))
  
  (define (square x)
  (* x x))
  
  (define (good-enough? guess pre-guess)
    (define tolerance 0.0000001)
    (< (abs (- guess pre-guess)) tolerance))
  
  (define (improve guess)
    (/ (+ (/ x (square guess)) (* 2.0 guess)) 3.0))
  
  (define (cbrt-iter guess pre-guess x)
    (if (good-enough? guess pre-guess)
        guess
        (cbrt-iter (improve guess) guess x)))
  (cbrt-iter 1.0 0.0 x))

;1.11
;recersive version
(define (f n)
  (cond ((>= n 3) (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))
        ((< n 3) n)))
;iteration vertion
(define (f-i n)
  (define (f-iter count n-1 n-2 n-3)
    (cond ((<  count 3) n-1)
          (else (f-iter (- count 1) (+ n-1 (* 2 n-2) (* 3 n-3)) n-1 n-2))))
  
  (if (< n 3)
      n
      (f-iter n 2 1 0)))

;1.12
(define (pascal-triangle r c)
  (cond ((= c 1) 1)
        ((= c r) 1)
        (else (+ (pascal-triangle (- r 1) (- c 1)) (pascal-triangle (- r 1) c)))))

;1.16
(define (expt b n)
  (define (square x)
    (* x x))
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

;1.17
(define (multi a b)
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  
  (cond
    ((= b 0) 0)
    ((even? b) (double (multi a (halve b))))
    (else (+ a (multi a (- b 1))))))

;1.18
(define (multi-iter-ver x y)
  (define (double x)
    (+ x x))
  (define (halve x)
    (/ x 2))
  (define (iter product x y)
    (cond ((= y 0) product)
          ((even? y) (iter product (double x) (halve y)))
          (else (iter (+ product x) x (- y 1)))))
  (iter 0 x y))


