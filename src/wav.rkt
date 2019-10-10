;; PCM data is 2s complement
(require threading)

(define (float->discrete s [fmin -1.0] [fmax 1.0] [dmin -32768] [dmax 32767])
  (let ([fdelta (- fmax fmin)]
	[ddelta (- dmax dmin)])
    (~>> s
	 (* (/ ddelta 2))
	 exact-floor)))

(define (chunk-fmt)
  (bytes-append (make-bytes 0)
		#"fmt "
		(integer->integer-bytes 16 4 #f #f)
		(integer->integer-bytes 1 2 #f #f)
		(integer->integer-bytes 1 2 #f #f)
		(integer->integer-bytes 44100 4 #f #f)
		(integer->integer-bytes (* 44100 2) 4 #f #f)
		(integer->integer-bytes 2 2 #f #f)
		(integer->integer-bytes (* 2 8) 2 #f #f)))

(define (chunk-data data)
  (let ([data-length (* 2 (length data))])
    (let ([dest (make-bytes (+ 4 4 data-length))])
      (bytes-copy! dest 0 #"data")
      (bytes-copy! dest 4 (integer->integer-bytes data-length 4 #f #f))
      (bytes-copy! dest 8 (~>> data
			       (map float->discrete)
			       (map (curryr integer->integer-bytes 2 #t #f))
			       (bytes-join _ #"")))
      dest)))

(define (raw->wav data)
  (let ([ckfmt (chunk-fmt)]
	[ckdata (chunk-data data)])
    (bytes-append (make-bytes 0)
		  #"RIFF"
		  (integer->integer-bytes (+ 4 (bytes-length ckfmt) (bytes-length ckdata)) 4 #f #f)
		  #"WAVE"
		  ckfmt
		  ckdata)))
