;;;; package.lisp --- Package definition for tests of the unparser module.
;;;;
;;;; Copyright (C) 2020-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.graphviz.unparser.test
  (:use
   #:cl

   #:fiveam)

  (:local-nicknames
   (#:bp       #:architecture.builder-protocol)

   (#:unparser #:language.graphviz.unparser)))

(cl:in-package #:language.graphviz.unparser.test)

(def-suite :language.graphviz.unparser
  :in :language.graphviz)
