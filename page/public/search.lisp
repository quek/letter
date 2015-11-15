(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defaction /public/search ()
  (with-public-template (:title #"""#,@q""")))
