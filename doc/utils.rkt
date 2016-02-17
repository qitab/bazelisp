#lang at-exp racket
;;-*- Scheme -*-

(require
  scribble/manual
  scribble/core
  scribble/decode
  scribble/base
  scriblib/autobib
  scribble/bnf
  (for-syntax syntax/parse))

(provide (all-defined-out))

(define (q . x) (list "\"" x "\""))

(define-syntax (clblock stx)
  (syntax-parse stx
    [(_ #:line-numbers ln str ...)
     #'@codeblock[;;#:keep-lang-line? #f
                   #:line-numbers ln
                   #:line-number-sep 3
                   str ...]]
    [(_ str ...)
     #'(clblock #:line-numbers 0 str ...)]))

(define-syntax (clcode stx)
  (syntax-parse stx
    [(_ str ...) #'(clblock #:line-numbers #f str ...)]))

(define (cl . str)
  (apply tt str))

(define (file . str)
  (apply tt str))

(define (CL) "Common Lisp")
(define (CLOS) "Common Lisp Object System")

(define (ASDF) (cl "ASDF"))

(define-syntax defpretty
  (lambda (stx)
    (syntax-case stx ()
      [(_ pretty name ...)
       (with-syntax ([(string ...) (map symbol->string (syntax->datum #'(name ...)))])
         #'(begin
             (define (name) (pretty string)) ...))])))

(defpretty tt
  lisp_binary lisp_library lisp_test cc_library
  srcs deps csrcs cdeps data compile_data
  order features nowarn visibility
)
(defpretty file
  BUILD WORKSPACE
)

(defpretty cl
  main cl-user::main
)

(define-cite ~cite cite-noun generate-bib) ;; #:style number-style)

(define-syntax-rule (define-bib name stuff ...)
  (define name (make-bib stuff ...)))
