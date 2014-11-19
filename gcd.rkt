#lang racket

(require test-engine/racket-tests)

;; (gcd a b) given two numbers, a and b, returns gcd(a, b)
;; gcd: Nat Nat Nat Nat -> Nat
;; Examples
(check-expect (my-gcd 15 5) 5)
(check-expect (my-gcd 15 6) 3)
(check-expect (my-gcd 15 0) 15)
(check-expect (my-gcd 0 0) 0)

(define (my-gcd a b)
  (cond
    [(zero? b) a]
    [else (my-gcd b (modulo a b))]))

(define (eea-bees-knees q0 r0 x0 y0
                        q1 r1 x1 y1)
  (local [(define q (quotient r0 r1))
          (define x (- x0 (* q x1)))
          (define y (- y0 (* q y1)))
          (define r (remainder r0 r1))
          (define printer
            (display (string-append 
                      (number->string x)
                      (string #\tab)
                      (number->string y)
                      (string #\tab)
                      (number->string q)
                      (string #\tab)
                      (number->string r)
                      (string #\newline))))]
         (cond
          [(zero? r) (list x0 y0 r1)]
          [else (eea-bees-knees q1 r1 x1 y1
                                q  r  x  y)])))

(define (eea a b)
  (eea-bees-knees 0 a 1 0
                  0 b 0 1))

;; Example (see http://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Example):
(eea 46 240)
