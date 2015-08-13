(in-package :memo)

;; テンプレート
(defmacro with-defalut-template (&body contents)
  `(html
     (raw "<!DOCTYPE html>")
     (:html
       :lang "ja"
       (:head (:meta :charset "UTF-8")
         (:title "memo")
         (:link :href "/stylesheets/main.css" :rel "stylesheet" :type "text/css"))
       (:body
        ,@contents))))

;; トップページ
(defaction /root (:path "/")
  (html
   (:h1 "メモ")
   (:p "メモ")))

(defvar *server*)

(defclass memo-app (application)
  ())

(defmethod call :around ((app memo-app))
  (with-db ((merge-pathnames "lepis/" *default-directory*))
    (call-next-method)))

;; start
(defun start (&key (port *http-port*))
  (unless *db*
    (setf *db* (open-db (merge-pathnames "lepis/" *default-directory*))))
  ;; html
  (setf info.read-eval-print.html:*html-pprint* nil)
  ;; Unpyo
  (setf *invoke-debugger-p* nil)
  (setq *server* (make-server :app (make-instance 'memo-app) :port port))
  (run *server*))

;; stop
(defun stop ()
  (stop *server*)
  (when *db*
    (close-db *db*)
    (setf *db* nil)))
