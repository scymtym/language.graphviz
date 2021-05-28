;;;; grammar.lisp --- Grammar rules for the graphviz language.
;;;;
;;;; Copyright (C) 2013-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Based on
;;;; http://cesta.sourceforge.net/dot/Dot.g
;;;; https://graphviz.org/doc/info/lang.html

(cl:in-package #:language.graphviz.parser)

(defrule skippable
    (+ (or #\Newline #\Space #\Tab)))

(defrule skippable?
    (* (or #\Newline #\Space #\Tab))
  (:error-report nil))

(macrolet ((define-keyword (name)
             (let ((rule-name (symbolicate '#:keyword- name)))
               `(parser.common-rules:defrule/s ,rule-name
                    (~ ,(string-downcase name))
                  (:constant ',name)))))
  (define-keyword strict)

  (define-keyword graph)
  (define-keyword digraph)

  (define-keyword subgraph)

  (define-keyword node)
  (define-keyword edge))

(macrolet ((define-delimiter (expression
                              &key
                              (rule-name  (symbolicate '#:delimiter- (string expression)))
                              (skippable? t)
                              (value      nil))
             `(,(if skippable?
                    'parser.common-rules:defrule/s
                    'esrap:defrule)
               ,rule-name
               ,expression
               (:constant ,value)))
           (define-delimiter-pair (open-expression close-expression)
             `(progn
                (define-delimiter ,open-expression)
                (define-delimiter ,close-expression :skippable? nil))))

  (define-delimiter      #\,  :rule-name delimiter-comma)
  (define-delimiter      #\;  :rule-name delimiter-semicolon :skippable? nil)
  (define-delimiter      #\:  :rule-name delimiter-colon)

  (define-delimiter      #\=)

  (define-delimiter-pair #\[ #\])
  (define-delimiter-pair #\{ #\})

  (define-delimiter      "--" :rule-name delimiter-line  :value :undirected)
  (define-delimiter      "->" :rule-name delimiter-arrow :value :directed))

;;; Names

(defrule/s name
    (+ (or (alpha-char-p character) #\_))
  (:text t))

;;; Attributes

(defrule symbol-literal
    (+ (character-ranges (#\a #\z) (#\A #\Z)))
  (:text t))

(defrule string/<>-delimited
    (and #\< (* (or (not (or #\< #\>)) string/<>-delimited)) #\>))

(defrule attribute-value
    (or parser.common-rules:float-literal
        parser.common-rules:integer-literal/decimal
        "\"\\N\""
        parser.common-rules:string-literal/double-quotes
        symbol-literal))

(defrule/s attribute
    (or (and name/?s delimiter-=/?s attribute-value)
        (and name    (and)          (and)))
  (:destructure (name equals value &bounds start end)
    (declare (ignore equals))
    (bp:node* (:attribute :name   name
                          :value  value
                          :bounds (cons start end)))))

(defrule another-attribute
    (and skippable? delimiter-comma/?s attribute)
  (:function third))

(defrule/s attributes
    (or (and attribute (* another-attribute))
        (and))
  (:destructure (&optional first rest)
    (when first
      (list* first rest))))

(defrule/s attribute-list
    (and delimiter-[/?s attributes/?s delimiter-])
  (:function second))

(defrule default-attributes
    (and (or keyword-graph/?s keyword-node/?s keyword-edge/?s)
         attribute-list
         delimiter-semicolon)
  (:destructure (scope attributes semicolon &bounds start end)
    (declare (ignore semicolon))
    (list (bp:node* (:default-attributes :scope scope :bounds (cons start end))
            (* :attribute attributes)))))

;;; Node

(defrule/s node-name
    (or name
        parser.common-rules:integer-literal/decimal/no-sign))

(defrule compass-direction
    (or "se" "sw" "ne" "nw" "n" "e" "s" "w" "c" "_"))

(defrule port
    (or (and delimiter-colon/?s node-name/?s delimiter-colon/?s compass-direction)
        (and delimiter-colon/?s node-name    (and)              (and))
        (and (and)              (and)        delimiter-colon/?s compass-direction))
  (:function rest)
  (:destructure (id colon direction &bounds start end)
    (declare (ignore colon))
    (bp:node* (:port :id id :direction direction :bounds (cons start end)))))

(defrule/s node-id
    (or (and node-name/?s port)
        (and node-name    (and)))
  (:destructure (id port &bounds start end)
    (bp:node* (:node-id :id     id
                        :bounds (cons start end))
      (bp:? :port port))))

(defrule node
    (and (and node-id/?s (? attribute-list/?s)) delimiter-semicolon)
  (:function first)
  (:destructure (id attributes &bounds start end)
    (list (bp:node* (:node :bounds (cons start end))
            (1 :id        id)
            (* :attribute attributes)))))

(defrule/s node-or-subgraph
    (or node-id subgraph))

;;; Edge

(defrule/s edge-rhs
    (and (or delimiter-line/?s delimiter-arrow/?s)
         (or (and (or node-id/?s subgraph/?s) edge-rhs)
             (and (or node-id    subgraph)    (and))))
  (:destructure (kind (target more))
    (list* (list kind target) more)))

(defrule edge
    (and (and node-or-subgraph/?s edge-rhs/?s (? attribute-list/?s))
         delimiter-semicolon)
  (:function first)
  (:destructure (start tos attributes)
    (loop :for from = start :then to
          :for (kind to) :in tos
          :collect (bp:node* (:edge :kind kind)
                     (1 :from      from)
                     (1 :to        to)
                     (* :attribute attributes)))))

;;; Graph

(defrule subgraph
    (or (and delimiter-{/?s (* element/?s) delimiter-})
        (and keyword-subgraph/?s (? id/?ws)
             delimiter-{/?s (* element/?s) delimiter-})
        (and keyword-subgraph/?s id)))

(defrule/s element
    (or parser.common-rules:c-style-comment/rest-of-line/trimmed
        edge
        default-attributes
        node
        subgraph))

(defrule element-list/?s
    (* element/?s)
  (:lambda (elements)
    (reduce #'nconc elements)))

(defrule/s graph-kind
    (and (? keyword-strict/s) (or keyword-graph keyword-digraph)))

(defrule/s graph-name
    (+ (character-ranges (#\a #\z) (#\A #\Z)))
  (:text t))

(defrule graph
    (and (or (and graph-kind/s  graph-name/?s)
             (and graph-kind/?s (and)))
         delimiter-{/?s element-list/?s delimiter-})
  (:destructure ((kind id) open elements close &bounds start end)
    (declare (ignore open close))
    (bp:node* (:graph :id id :kind kind :bounds (cons start end))
      (* :statement elements))))
