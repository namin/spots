(define (c) 123)

(define (sub a b) (- a b))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (factiter acc n)
  (if (= n 0)
      acc
      (factiter (* acc n) (- n 1))))

(define (fact n) (factiter 1 n))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fibiter a b n)
  (if (= n 0)
      b
      (fibiter b (+ a b) (- n 1))))

(define (dumb x)
  (if (if (< x 2) (= x 0) (= x 2))
      0
      1))

(define (dumb2 x)
  (if (if (if (< x 2) (= x 0) (= x 2)) (= x 0) (= x 4))
      0
      1))

(define (dumb3 x)
  (if x
      (= 1 0)
      (= 1 1)))