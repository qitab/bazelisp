#lang at-exp racket

(require scribble/base scribble/manual scriblib/autobib "utils.rkt")

(provide (all-defined-out))

(define-bib XCVB-2009
  #:author "François-René Rideau and Spencer Brody"
  #:title "XCVB: an eXtensible Component Verifier and Builder for Common Lisp"
  ;; #:url "http://common-lisp.net/projects/xcvb/"
  ;; International Lisp Conference
  #:date "2009")

(define-bib ASDF3-2014
  #:title "ASDF3, or Why Lisp is Now an Acceptable Scripting Language (extended version)"
  #:author "François-René Rideau"
  ;; #:url "http://fare.tunes.org/files/asdf3/asdf3-2014.html"
  #:date "2014")

(define-bib Inside-Orbitz
  #:title "Carl de Marcken: Inside Orbitz"
  #:author (author-name "Carl" "de Marcken")
  #:url "http://www.paulgraham.com/carl.html"
  #:date "2001")

(define-bib Bazel
  #:title "Bazel"
  #:author "Google"
  #:url "http://bazel.io/"
  #:date "2015")

(define-bib Bazelisp
  #:title "Bazelisp"
  #:author "Google"
  ;;#:url "http://github.com/qitab/bazelisp"
  #:date "2016")

(define-bib FASTEVAL
  #:title "SBCL's Fasteval interpreter"
  #:author "Douglas Katzman"
  ;; #:url "https://github.com/sbcl/sbcl/tree/master/src/interpreter"
  #:date "2015")

(define-bib in-the-large-small
  #:title "Programming-in-the-large versus Programming-in-the-small"
  #:author "Frank DeRemer and Hans Kron"
  ;; #:url "http://www.cs.umd.edu/class/spring2005/cmsc838p/General/pitl.pdf"
  ;; IEEE Transactions on Software Engineering 2(2)
  #:date "1975")
