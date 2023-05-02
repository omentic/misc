#lang racket

(require racket/cmdline)
(require "src/formatter.rkt")

; (define mangle-parenthesis? (make-parameter #t))
; (define mangle-newlines? (make-parameter #t))

(command-line
  #:program "antifmt"
  #:once-each
  [("-p" "--mangle-parens")
    "Toggle mangling of parenthesis: on by default"
    (mangle-parens? #f)]
  [("-i" "--mangle-indents")
    "Toggle mangling of indentation: off by default"
    (mangle-indents? #t)]
  [("-n" "--mangle-newlines")
    "Toggle mangling of newlines: on by default"
    (mangle-newlines? #f)]
  #:args (loc)
  (display
    (string-append ; what the fuck
      (mangle (parameterize ([read-accept-lang #t] [read-accept-reader #t])
        (sequence->list (in-producer read eof
          (open-input-file loc)))))
      "\n")))
