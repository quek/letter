(in-package :letter)

(defparameter *default-directory*
  (pathname (directory-namestring #.(or *compile-file-truename* *load-truename*)))
  "stylesheets, images, db ディレクトリを配置するディレクトリ。")

(defparameter *http-port* 1959 "HTTP のポート。")

(defparameter *swank-port* 4009 "SLIME のポート。")

(setf unpyo::*session-secret* "hello world!")
