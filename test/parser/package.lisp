;;;; package.lisp --- Package definition for tests of the parser module.
;;;;
;;;; Copyright (C) 2020-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.graphviz.parser.test
  (:use
   #:cl

   #:fiveam)

  (:import-from #:language.graphviz.parser
   #:attribute
   #:attribute-list
   #:node
   #:edge)

  (:local-nicknames
   (#:a  #:alexandria)))

(cl:in-package #:language.graphviz.parser.test)

(def-suite :language.graphviz.parser
  :in :language.graphviz)
