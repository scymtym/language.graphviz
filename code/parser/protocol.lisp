;;;; protocol.lisp --- Protocol provided by the parser module.
;;;;
;;;; Copyright (C) 2013-2020 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.graphviz.parser)

(defgeneric parse (builder input &key rule))

;;; Default behavior

(defmethod parse ((builder t) (input string) &key (rule 'graph))
  (bp:with-builder (builder)
    (esrap:parse rule input)))
