#lang scheme
(require (planet soegaard/sicp:2:1/sicp))

;utilites
(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (define (next-skip-even t)
    (if (= n 2) 3 (+ t 2)))
  (define (next t)
    (+ 1 t))
  (cond ((> (square test-divisor) n) n)
        ((divisor? test-divisor n) test-divisor)
        (else (find-divisor n (next-skip-even test-divisor)))))

(define (divisor? b a)
  (= 0 (remainder a b)))

(define (prime? n)
  (= n (smallest-divisor n)))


(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) 
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try a)
    (= (remainder (expmod a n n) n) (remainder a n)))
  (try (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (get-sum f a next b)
  (if (> a b)
      0.0
      (+ (f a) (get-sum f (next a) next b))))

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

;1.19
(define (fast-fib n)
  (define (square x) (* x x))
  
  (define (transP p q)
    (+ (square p) (square q)))
  
  (define (apply-trans-a p q a b)
    (+ (* b q) (* a q) (* a p)))
  
  (define (apply-trans-b p q a b)
    (+ (* b p) (* a q)))
  
  (define (transQ p q)
    (+ (* 2 (* p q)) (square q)))
  
  (define (iter a b p q n)
    (cond ((= n 0) b)
          ((even? n) (iter a b (transP p q) (transQ p q) (/ n 2)))
          (else (iter (apply-trans-a p q a b)
                      (apply-trans-b p q a b)
                      p 
                      q
                      (- n 1)))))
  (iter 1 0 0 1 n))

;1.21
;(smallest-divisor 199)
;(smallest-divisor 1999)
;(smallest-divisor 19999)

;1.22
(define (timed-prime-test n)
  (time (fast-prime? n 100)))

(define (search-for-primes n amounts)
  (if (> amounts 0)
      (cond ((timed-prime-test n) 
             (display n)
             (display "\n")
             (search-for-primes (+ n 2) (- amounts 1)))
            (else (search-for-primes (+ n 2) amounts))
        )
      null))

;1.23 the ratio is about 7:4. Because the 'if', it doesn't halve the time.


;1.29
(define (integral f a b dx)
  (define (add-dx a) (+ a dx))
  (* (get-sum f (+ a (/ dx 2.0)) add-dx b)
     dx))


(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (add-2h x) (+ x (* 2.0 h)))
  (* (/ h 3.0)
     (+ (f a)
        (* 4.0 (get-sum f (+ a h) add-2h (- b h)))
        (* 2.0 (get-sum f (+ a (* 2.0 h)) add-2h (- b (* 2 h))))
        (f b))))

;1.30
(define (get-sum-iter f a next b)
  (define (iter a product)
    (if (> a b)
        product
        (iter (next a) (+ product (f a)))))
  (iter a 0))

;1.31
(define (get-product-iter f a next b)
  (define (iter a product)
    (if (> a b)
        product
        (iter (next a) (* product (f a)))))
  (iter a 1.0))

(define (get-product f a next b)
  (if (> a b)
      1.0
      (* (f a) (get-product f (next a) next b))))

(define (factorial n)
  (get-product-iter (lambda (x) x) 1 (lambda (x) (+ x 1)) n))

(define (qut-pi n)
  (* (get-product-iter (lambda (a) (/ a (+ a 1.0))) 2 (lambda (x) (+ x 2.0)) n)
     (get-product-iter (lambda (a) (/ a (- a 1.0))) 4 (lambda (x) (+ x 2.0)) n)))

(define my-pi (* 4.0 (qut-pi 1000)))

