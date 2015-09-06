(defpackage :memo
  (:use :cl
        :unpyo
        :info.read-eval-print.html
        :lepis)
  (:shadow :unpyo #:stop)
  (:export #:start
           #:stop))
