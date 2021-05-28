;;;; grammar.lisp --- Test for grammar rules of the parser module.
;;;;
;;;; Copyright (C) 2013-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.graphviz.parser.test)

(in-suite :language.graphviz.parser)

(defmacro define-rule-test (rule &body cases)
  (let ((test-name (a:symbolicate '#:rule. rule)))
    `(test ,test-name
       ,(format nil "Smoke test for the `~(~A~)' rule." rule)
       (architecture.builder-protocol:with-builder ('list)
         (parser.common-rules.test:parses-are (,rule) ,@cases)))))

;;; Attributes

(define-rule-test attribute
  ("foo"   '(:attribute () :name "foo" :value nil :bounds (0 . 3)))
  ("foo=1" '(:attribute () :name "foo" :value 1   :bounds (0 . 5))))

(define-rule-test attribute-list
  ("[]"          '())
  ("[foo,bar=1]" '((:attribute () :name "foo" :value nil :bounds (1 . 4))
                   (:attribute () :name "bar" :value 1   :bounds (5 . 10)))))

;;; Nodes

(define-rule-test node
  ("1;"     '((:node
               (:id (((:node-id () :id 1 :bounds (0 . 1)))))
               :bounds (0 . 2))))
  ("foo;"   '((:node
               (:id (((:node-id () :id "foo" :bounds (0 . 3)))))
               :bounds (0 . 4))))
  ("foo:1;" '((:node
               (:id (((:node-id
                       (:port (((:port () :id 1 :direction nil :bounds (3 . 5)))))
                       :id     "foo"
                       :bounds (0 . 5)))))
               :bounds (0 . 6))))
  ("foo[bar=1];"
   '((:node
      (:id        (((:node-id () :id "foo" :bounds (0 . 3))))
       :attribute (((:attribute () :name "bar" :value 1 :bounds (4 . 9)))))
      :bounds (0 . 11))))
  ("foo:10:sw[bar=1];"
   '((:node
      (:id        (((:node-id
                     (:port (((:port () :id 10 :direction "sw" :bounds (3 . 9)))))
                     :id "foo" :bounds (0 . 9))))
       :attribute (((:attribute () :name "bar" :value 1 :bounds (10 . 15)))))
      :bounds (0 . 17)))))

;;; Edges

(define-rule-test edge
  ("1->2;"
   '((:edge
      (:from (((:node-id () :id 1 :bounds (0 . 1))))
       :to   (((:node-id () :id 2 :bounds (3 . 4)))))
      :kind :directed)))
  ("1->2->3[foo=bar];"
   '((:edge
      (:from      (((:node-id () :id 1 :bounds (0 . 1))))
       :to        #1=(((:node-id () :id 2 :bounds (3 . 4))))
       :attribute #2=(((:attribute () :name "foo" :value "bar" :bounds (8 . 15)))))
      :kind :directed)
     (:edge
      (:from      #1#
       :to        (((:node-id () :id 3 :bounds (6 . 7))))
       :attribute #2#)
      :kind :directed))))
