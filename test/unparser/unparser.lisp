;;;; grammar.lisp --- Test for grammar rules of the unparser module.
;;;;
;;;; Copyright (C) 2013-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.graphviz.unparser.test)

(in-suite :language.graphviz.unparser)

(test unparse.smoke
  "Smoke test for the `unparse' function."

  (let ((ast (bp:with-builder ('list)
               (bp:node* (:graph :kind '(nil :digraph))
                 (1 :statement (bp:node* (:node)
                                 (1 :id        (bp:node* (:node-id :id "a")))
                                 (1 :attribute (bp:node* (:attribute :name "b" :value "c")))))
                 (1 :statement (bp:node* (:edge :kind :directed)
                                 (1 :from (bp:node* (:node-id :id "a")))
                                 (1 :to   (bp:node* (:node-id :id "c")))))))))
    (is (string= (format nil "digraph {~@
                              ~2@Ta [b = \"c\"];~@
                              ~2@Ta -> c;~@
                              ~@
                              }")
                 (with-output-to-string (stream)
                   (let ((*print-pretty* t))
                     (unparser:serialize 'list ast stream)))))))
