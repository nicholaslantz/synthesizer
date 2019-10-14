#lang racket

(define +sample-rate+ 44100)
(define +concert-pitch+ 440)

;; FIXME:  What should I do with this?  It may belong in a vfx
;; module, but it's also a wave-op...
(define (level w shape)
  (lambda (t)
    (* (shape t)
       (w t))))



