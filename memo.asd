(asdf:defsystem :memo
  :version "0.0.1"
  :serial t
  :components ((:file "packages")
               (:file "config")
               (:file "memo"))
  :depends-on (:info.read-eval-print.html
               :unpyo
               :lepis
               :info.read-eval-print.double-quote))
