#lang racket

(provide sine
	 square
	 sawtooth
	 triangle
	 flatline)

(define (sine x)
  (sin (* 2 pi x)))

(define (fractional x)
  (- x (floor x)))

(define (square x)
  (sub1 (* 2 (round (fractional x)))))

(define (sawtooth x)
  (sub1 (* 2 (fractional x))))

;; TODO: Remove `if`
(define (triangle x)
  (let* ([p (fractional x)])
    (if (< p 0.5)
	(sub1 (* p 4))
	(add1 (- (* (- p 0.5) 4))))))

(define (flatline x)
  0)
