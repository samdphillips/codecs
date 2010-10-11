#lang racket

(struct codec
  (encode decode) #:transparent)

(define (encode-to-port codec v p)
  ((codec-encode codec) v p))

(define (decode-from-port codec p)
  ((codec-decode codec) p))

(define (encode-to-bytes codec v)
  (call-with-output-bytes
   (lambda (port)
     (encode-to-port codec v port))))

(define (decode-from-bytes codec b)
  (call-with-input-bytes b
    (lambda (port)
      (decode-from-port codec port))))

(define (string/codec size/codec)
  (codec
   (lambda (v p)
     (let ([v (string->bytes/utf-8 v)])
       (encode-to-port size/codec (bytes-length v) p)
       (write-bytes v p)))
   (lambda (p)
     (let* ([size (decode-from-port size/codec p)]
            [b   (read-bytes size p)])
       (bytes->string/utf-8 b)))))

(define (integer/codec size signed? [big-endian? (system-big-endian?)])
  (codec
   (lambda (v p)
     (write-bytes
      (integer->integer-bytes v size signed? big-endian?) p))
   (lambda (p)
     (let ([b (read-bytes size p)])
       (integer-bytes->integer b signed? big-endian?)))))

(define int32/codec (integer/codec 4 #t))
(define uint32/codec (integer/codec 4 #f))

(provide (all-defined-out))