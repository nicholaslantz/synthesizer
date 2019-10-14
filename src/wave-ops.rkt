#lang racket

(define (pitch frequency wave)
  (lambda (t)
    (wave (* frequency t))))

(define (scale amount wave)
  (lambda (t)
    (* amount (wave t))))

(define (shift amount wave)
  (lambda (t)
    (+ amount (wave t))))

;; FIKME: This function is broken.
(define (osc shape [frequency 440] [level 1] [offset 0])
  (compose (curry shift offset)
	   (curry scale level)
	   (curry pitch frequency)
	   shape))

(define (mix waves [levels (make-list (length waves) (/ (length waves)))])
  (let ( ; TODO: normalize
	[normalized levels])
    (lambda (t)
      (apply +
	     (map (lambda (w l) (* l (w t)))
		  waves
		  normalized)))))

;; TODO: Don't like how imperative this is, find
;;       functional form
(define (sequence waves durations)
  (let ([active (first waves)]
	[index 0]
	[next-at (first durations)])
    (lambda (t)
      (begin
	(when (>= t next-at)
	  (set! index (remainder (add1 index) (length waves)))
	  (set! active (list-ref waves index))
	  (set! next-at (+ next-at (list-ref durations index))))
	(active t)))))
