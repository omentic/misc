#lang racket

(require racket/cmdline)
(provide mangle mangle-parens? mangle-indents? mangle-newlines?)

(define (random-paren)
  (match (random 0 3)
    (0 (list "(" ")"))
    (1 (list "[" "]"))
    (2 (list "{" "}"))))

(define (random-newline)
  (make-string (random 0 3) #\newline))

(define (random-indentation at-least)
  (make-string (+ at-least (random 0 3)) #\space))

(define indent 0)
(define mangle-parens? (make-parameter #t))
(define mangle-indents? (make-parameter #f))
(define mangle-newlines? (make-parameter #t))

; horrid hack
(define (mangle sexp)
  (string-append "#lang racket \n"
    (mangle-sexp (rest (first (rest (rest (rest (first sexp)))))))))

(define (mangle-sexp sexp)
  (set! indent (+ indent 2))
  (if (not (list? sexp)) (error 'mangle "not an s-exp")
    (if (empty? sexp) ""
      (let ((paren (random-paren)))
        (string-append
          (if (mangle-indents?) (random-indentation indent) "")
          (if (mangle-parens?) (first paren) ")")
          (mangle-params sexp)
          (if (mangle-newlines?) (random-newline) "")
          (if (mangle-indents?) (random-indentation indent) "")
          (if (mangle-parens?) (last paren) ")"))))))

(define (mangle-params sexp)
  (set! indent (max (- indent 2) 0))
  (if (not (list? sexp)) (~a sexp)
    (if (empty? sexp) ""
      (let ((param (first sexp)))
        (string-join (list
          (if (list? param)
            (mangle-sexp param)
            (if (string? param)
              (string-append "\"" (~a param) "\"")
              (~a param)))
          (mangle-params (rest sexp))))))))
