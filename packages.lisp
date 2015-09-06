(info.read-eval-print.series-ext:sdefpackage :memo
  (:use :cl
        :unpyo
        :info.read-eval-print.html
        :lepis)
  (:shadow :unpyo #:stop)
  (:export #:start
           #:stop))
