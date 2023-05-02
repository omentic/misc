#lang racket

(define (mangle sexp)
  (if (not (list? sexp)) (error 'mangle "not an s-exp")
    (if (empty? sexp) ""
      (let ((paren (random-paren)))
        (string-append
          (first paren)
          (mangle-params sexp)
          (random-newline)
          (last paren))))))
