#lang racket

(define +sample-rate+ 44100)
(define +concert-pitch+ 440)

;; FIXME: This function is an unholy abomination.  Revise with
;; lambda-case
(define (note name [octave 4])
  (let ([notes (map (lambda (n c)
		      (cons n (* +concert-pitch+ (cents c))))
		    '(c d e f g a b)
		    '(-900 -700 -500 -400 -200 0 200))]
	[o (- octave 4)]
	[n (let ([sharps-flats (cdr (string->list (symbol->string name)))])
	     (if (empty? sharps-flats)
		 0
		 (* 100 (+ (count (curry char-ci=? #\#) sharps-flats)
			   (- (count (curry char-ci=? #\b) sharps-flats))))))]
	[m (string->symbol (string (string-ref (symbol->string name) 0)))])
    (* (expt 2 o) (cdr (assoc m notes)) (cents n))))

(define (interval name)
  (let ([intervals (map (lambda (i)
			  (cons (car i) (cents (cdr i))))
			'((P1 . 0)   (m2 . 100) (M2 . 200)  (m3 . 300)
			  (M3 . 400) (P4 . 500) (TT . 600)  (P5 . 700)
			  (m6 . 800) (M6 . 900) (m7 . 1000) (M7 . 1100)
			  (P8 . 1200)))])
    (cdr (assoc name intervals))))

(define (cents a)
  (expt 2 (/ a 1200)))

(define (chord shape root . intervals)
  (mix (cons (pitch root shape)
	     (map (lambda (i)
		    (pitch (* root i) shape))
		  intervals))))

(define (triad-major shape root)
  (chord shape root
	 (interval 'M3)
	 (interval 'P5)))

(define (triad-minor shape root)
  (chord shape root
	 (interval 'm3)
	 (interval 'P5)))

;; FIXME:  What should I do with this?  It may belong in a vfx
;; module, but it's also a wave-op...
(define (level w shape)
  (lambda (t)
    (* (shape t)
       (w t))))



