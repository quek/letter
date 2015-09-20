(info.read-eval-print.series-ext:sdefpackage :memo
  (:use :cl
        :hu.dwim.defclass-star
        :anaphora
        :unpyo
        :info.read-eval-print.html
        :lepis)
  (:shadow :unpyo #:stop)
  (:export #:start
           #:stop))
