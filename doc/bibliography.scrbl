#lang at-exp racket

(require scribble/base scribble/manual scriblib/autobib "utils.rkt")

(provide (all-defined-out))

(define-bib CHINE-NUAL
  #:title "Lisp Machine Manual"
  #:author "Dan Weinreb and David Moon"
  ;;#:publisher "MIT"
  #:date "1981"
  #:url "https://bitsavers.trailing-edge.com/pdf/mit/cadr/chinual_4thEd_Jul81.pdf")

(define-bib Pitman-Large-Systems
  #:author "Kent Pitman"
  #:title "The Description of Large Systems"
  #:date "1984" ;; September
  ;; #:type "MIT AI Memo"
  ;; #:number "801"
  ;; #:institution "MIT AI Lab"
  #:url "http://www.nhplace.com/kent/Papers/Large-Systems.html")

(define-bib AITR-874
  #:author "Richard Elliot Robbins"
  #:title "BUILD: A Tool for Maintaining Consistency in Modular Systems"
  #:date "1985" ;; #:month "November"
  ;; #:institution "MIT AI Lab"
  ;; #:type "MIT AI TR"
  ;; #:number "874"
  #:url "ftp://publications.ai.mit.edu/ai-publications/pdf/AITR-874.pdf")

(define-bib MK-DEFSYSTEM
  #:author "Mark Kantrowitz"
  #:title "Defsystem: A Portable Make Facility for Common Lisp"
  #:date "1990" ;; January 1990.
  ;; School of Computer Science. Carnegie Mellon University.
  #:url "ftp://ftp.cs.rochester.edu/pub/archives/lisp-standards/defsystem/pd-code/mkant/defsystem.ps.gz")

(define-bib Critique-DIN-Kernel-Lisp
  #:author "Henry Baker"
  #:title "Critique of DIN Kernel Lisp Definition Version 1.2"
  #:url "http://www.pipeline.com/~hbaker1/CritLisp.html"
  #:date "1992")

(define-bib Cult-of-Dead-mail
  #:title "Cult of Dead"
  #:author "Jim Benson"
  #:date "2002"
  #:url "http://wiki.squeak.org/squeak/2950")

(define-bib ASDF-Manual
  #:title "ASDF Manual"
  #:author "Daniel Barlow" ;; and contributors?
  #:date "2004" ;; 2001—2014
  #:url "http://common-lisp.net/project/asdf/")

(define-bib faslpath-page
  #:title "faslpath"
  #:author "Peter von Etter"
  #:date "2009"
  #:url "https://code.google.com/p/faslpath/")

(define-bib XCVB-2009
  #:author "François-René Rideau and Spencer Brody"
  #:title "XCVB: an eXtensible Component Verifier and Builder for Common Lisp"
  #:url "http://common-lisp.net/projects/xcvb/"
  ;; International Lisp Conference
  #:date "2009")

(define-bib Software-Irresponsibility
  #:author "François-René Rideau"
  #:title "Software Irresponsibility"
  #:date "2009"
  #:url "http://fare.livejournal.com/149264.html")

(define-bib Evolving-ASDF
  #:author "François-René Rideau and Robert Goldman"
  #:title "Evolving ASDF: More Cooperation, Less Coordination"
  #:date "2010"
  #:url "http://common-lisp.net/project/asdf/doc/ilc2010draft.pdf")

(define-bib quicklisp
  #:author "Zach Beane"
  #:title "Quicklisp"
  #:date "2011"
  #:url "http://quicklisp.org/") ;; also see blog.quicklisp.org and xach.livejournal.com archives

;; ASDF 2.26: http://fare.livejournal.com/170504.html

;; <nyef> Looks like quick-build was first implemented on 2012-04-02, so just over two years ago, but there was about a year of bugfixing to get its current form.
;; <nyef> Well, bugfixing and feature enhancement.
(define-bib Quick-build
  #:title "Quick-build (private communication)"
  #:author "Alastair Bridgewater"
  #:date "2012")

(define-bib ASDF3-2014
  #:title "ASDF3, or Why Lisp is Now an Acceptable Scripting Language (extended version)"
  #:author "François-René Rideau"
  #:date "2014"
  #:url "http://fare.tunes.org/files/asdf3/asdf3-2014.html")

(define-bib Lisp-Acceptable-Scripting-Language
  #:title "Why Lisp is Now an Acceptable Scripting Language"
  #:author "François-René Rideau"
  #:date "2014"
  ;; European Lisp Symposium, 2014-05-05
  #:url "http://github.com/fare/asdf3-2013/")

(define-bib Inside-Orbitz
  #:title "Carl de Marcken: Inside Orbitz"
  #:author "Carl de Marcken"
  #:date "2001"
  #:url "http://www.paulgraham.com/carl.html")

(define-bib Bazel
  #:title "Bazel"
  #:author "Google"
  #:date "2015"
  #:url "http://bazel.io/")

(define-bib Bazelisp
  #:title "Bazelisp"
  #:author "Google"
  #:date "2016"
  #:url "http://github.com/qitab/bazelisp")
