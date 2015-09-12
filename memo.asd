(asdf:defsystem :memo
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "memo"))
  :depends-on (:info.read-eval-print.series-ext
               :info.read-eval-print.double-quote
               :info.read-eval-print.html
               :cl-css
               :3bmd
               :3bmd-ext-code-blocks
               :parenscript
               :unpyo
               :lepis))
