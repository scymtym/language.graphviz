;;;; package.lisp --- Package definition for language.graphviz.parser system.
;;;;
;;;; Copyright (C) 2013-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.graphviz.parser
  (:use
   #:cl
   #:alexandria
   #:let-plus)

  (:local-nicknames
   (#:bp #:architecture.builder-protocol))

  (:import-from #:esrap
   #:defrule
   #:&bounds
   #:character-ranges
   #:~ #:?)

  (:import-from #:parser.common-rules
   #:defrule/s)

  ;; Rules
  (:export
   #:graph)

  ;; Parse protocol
  (:export
   #:parse)

  (:documentation
   "This package contains a parser for the GraphViz dot language."))
