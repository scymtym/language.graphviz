;;;; unparser.lisp --- Unparser for the graphviz language.
;;;;
;;;; Copyright (C) 2016-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:language.graphviz.unparser)

(defun serialize (builder tree stream)
  (labels ((process-node (stream recurse
                          relation relation-args
                          node kind relations &rest initargs)
             (declare (ignore relation relation-args))
             (labels ((attribute-processor (stream)
                        (let ((first? t))
                          (lambda (&rest args)
                            (if first?
                                (setf first? nil)
                                (write-string ", " stream))
                            (apply #'process-node stream args))))
                      (process-attributes (recurse node relations stream)
                        (when (member :attribute relations)
                          (write-string " [" stream)
                          (pprint-logical-block (stream (list node))
                            (funcall recurse
                                     :function  (attribute-processor stream)
                                     :relations '(:attribute)))
                          (write-string "]" stream))))
               (case kind
                 (:graph
                  (destructuring-bind (&key id ((:kind (&optional strict kind)))
                                       &allow-other-keys)
                      initargs
                    (format stream "~(~@[~A ~]~A~@[ ~A~]~) {~@:_~2@T"
                            strict kind id)
                    (pprint-logical-block (stream (list node))
                      (funcall recurse :function (a:curry #'process-node stream)))
                    (format stream "~@:_}")))
                 (:default-attributes
                  (destructuring-bind (&key scope &allow-other-keys) initargs
                    (format stream "~(~A~) " scope)
                    (process-attributes recurse node relations stream)
                    (write-char #\; stream))
                  (pprint-newline :mandatory stream))
                 (:node
                     (pprint-logical-block (stream (list node))
                       (funcall recurse :relations '(:id))
                       (process-attributes recurse node relations stream)
                       (write-char #\; stream))
                   (pprint-newline :mandatory stream))
                 (:edge
                  (pprint-logical-block (stream (list node))
                    (funcall recurse :function  (a:curry #'process-node stream)
                                     :relations '(:from))
                    (write-string (ecase (getf initargs :kind)
                                    (:undirected " -- ")
                                    (:directed   " -> "))
                                  stream)
                    (funcall recurse :function  (a:curry #'process-node stream)
                                     :relations '(:to))
                    (process-attributes recurse node relations stream)
                    (write-char #\; stream))
                  (pprint-newline :mandatory stream))
                 (:node-id
                  (destructuring-bind (&key id port &allow-other-keys) initargs
                    (format stream "~A~@[:~A~]" id port)))
                 (:attribute
                  (destructuring-bind (&key name value &allow-other-keys) initargs
                    (typecase value
                      (float
                       (let ((*read-default-float-format* (type-of value)))
                         (format stream "~A = ~S" name value)))
                      (t
                       (format stream "~A = ~S" name value)))))))))
    (pprint-logical-block (stream (list tree))
      (bp:walk-nodes builder (a:curry #'process-node stream) tree))))
