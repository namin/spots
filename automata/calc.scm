(define (sub a b) (- a b))

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(define (factiter acc n)
  (if (= n 0)
      acc
      (factiter (* acc n) (- n 1))))

(define (fib n)
  (if (< n 2)
      1
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (fibiter a b n)
  (if (= n 0)
      b
      (fibiter b (+ a b) (- n 1))))

(define (c) 123)