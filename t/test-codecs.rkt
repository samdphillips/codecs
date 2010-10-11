#lang planet samdphillips/testy:1
"Codec tests"

(require "../codecs.rkt")

(let ([codec (string/codec uint32/codec)])
  (check-equal? (decode-from-bytes codec (encode-to-bytes codec "abc123"))
                "abc123"))
