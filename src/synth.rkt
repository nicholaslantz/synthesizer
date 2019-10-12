(require threading)

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

;; Note sin is also an available shapes

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
(define (osc [shape sine] [frequency 440] [level 1] [offset 0])
  (compose (curry shift offset)
	   (curry scale level)
	   (curry pitch frequency)
	   shape))

;; Need to adjust this to take levels as well.  I can take
;; waves ar a list instead of as a rest, I could alternate
;; between sounds and levels, Hmmm...
(define (mix waves [levels (make-list (length waves) (/ (length waves)))])
  (let (; TODO: normalize
	[normalized levels])
    (lambda (t)
      (apply +
	     (map (lambda (w l) (* l (w t)))
		  waves
		  normalized)))))

(define (cents a)
  (expt 2 (/ a 1200)))

(define (chord shape root . intervals)
  (apply mix (cons (wave shape root)
		   (map (lambda (i)
			  (wave shape (* root i)))
			intervals))))

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

(define (triad-major shape root)
  (chord shape root
	 (interval 'M3)
	 (interval 'P5)))

(define (triad-minor shape root)
  (chord shape root
	 (interval 'm3)
	 (interval 'P5)))

(define (level w shape)
  (lambda (t)
    (* (shape t)
       (w t))))

(define (wave->signal w duration)
  (map w (range duration)))

(define (mix-signals . signals)
  (let* ([mixed (map (curry apply +)
		     (apply (curry map list) signals))]
	 [maximum (apply max (map abs mixed))])
    (map (curryr / maximum) mixed)))


