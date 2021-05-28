;;;; package.lisp --- Package definition for tests of the language.graphviz system.
;;;;
;;;; Copyright (C) 2020-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:language.graphviz.test
  (:use
   #:cl
   #:fiveam)

  (:export
   #:run-test))

(cl:in-package #:language.graphviz.test)

(def-suite :language.graphviz)

(defun run-tests ()
  (run! :language.graphviz))
