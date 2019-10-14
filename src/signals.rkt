#lang racket
(provide +sample-rate+
	 wave->signal
	 mix-signals)

(define +sample-rate+ 44100)

(define (wave->signal wave duration)
  (map wave (range 0 duration (/ +sample-rate+))))

(define (mix-signals . signals)
  (let* ([mixed (map (curry apply +)
		     (apply (curry map list) signals))]
	 [maximum (apply max (map abs mixed))])
    (map (curryr / maximum) mixed)))
