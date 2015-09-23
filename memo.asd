(asdf:defsystem :memo
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "memo"))
  :depends-on (:info.read-eval-print.series-ext
               :info.read-eval-print.double-quote
               :info.read-eval-print.html
               :info.read-eval-print.lambda
               :cl-css
               :3bmd
               :3bmd-ext-code-blocks
               :anaphora
               :parenscript
               :oauth2
               :diff
               :cl-ppcre
               :hu.dwim.defclass-star
               :unpyo
               :lepis))
