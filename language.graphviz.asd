;;;; language.graphviz.asd --- System definition for the language.graphviz system.
;;;;
;;;; Copyright (C) 2012-2021 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;; System definition

(defsystem "language.graphviz"
  :description "Parsing and unparsing of the Graphviz language."
  :license     "GPLv3" ; see COPYING file for details.

  :author      #1="Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  #1#

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ()

  :components  ()

  :in-order-to ((test-op (test-op "language.graphviz/test"))))

(defsystem "language.graphviz/test"
  :description "Unit tests for the language.graphviz system"
  :license     "GPLv3" ; see COPYING file for details.

  :version     (:read-file-form "version-string.sexp")
  :depends-on  ("fiveam")

  :components  ((:module     "test"
                 :components ((:file       "package"))))

  :perform     (test-op (operation component)
                 (uiop:symbol-call '#:language.graphviz.test '#:run-tests)))
