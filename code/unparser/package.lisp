;;;; package.lisp --- Package definition for the unparser module.
;;;;
;;;; Copyright (C) 2016-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.graphviz.unparser
  (:use
   #:cl
   #:alexandria)

  (:local-nicknames
   (#:a  #:alexandria)

   (#:bp #:architecture.builder-protocol))

  (:export
   #:serialize)

  (:documentation
   "This package contains an unparser for the GraphViz dot language."))
