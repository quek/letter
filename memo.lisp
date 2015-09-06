(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

;; テンプレート
(defmacro with-defalut-template ((&key (title "memo")) &body contents)
  `(html
     (:!doctype :html t)
     (:html
       :lang "ja"
       (:head (:meta :charset "UTF-8")
         (:title ,title)
         (:link :href "/main.css" :rel "stylesheet" :type "text/css"))
       (:body
           (:div (:a :href "/" "トップ"))
         ,@contents))))

;; トップページ
(defaction /root (:path "/")
  (with-defalut-template ()
    (:h1 "メモ")
    (:ul
        (dolist (title (zrang "titles" 0 nil))
          (html
            (:li
                (:a :href #"""/show/#,title""" title)))))))

(defaction /main.css ()
  (setf (content-type) "text/css")
  (format *html-output* "
body {
  color: #333;
}
textarea.edit {
  width: 80%;
  height: 400px;
}
"))

(defun doc-key (title)
  (format nil "doc ~a" title))

(defaction /edit/@title ()
  (let ((doc (@ (doc-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (:form :action #"""/save/#,@title""" :method "post"
        (:textarea.edit :name "doc" doc)
        (:p (:input :type "submit" :value "save"))))))

(defaction /save/@title (:method :post)
  (! (doc-key @title) @doc)
  (zadd "titles" (get-universal-time) @title)
  (redirect (format nil "/show/~a" @title)))

(defaction /show/@title ()
  (let ((doc (@ (doc-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (:pre doc)
      (:p (:a :href (format nil "/edit/~a" @title) "編集")))))

(defvar *server*)

(defclass memo-app (application)
  ())

(defmethod call :around ((app memo-app))
  (with-db ((merge-pathnames "lepis/" *default-directory*))
    (call-next-method)))

;; start
(defun start (&key (port *http-port*))
  (format t "http://localhost:~d~%" port)
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
  (unpyo:stop *server*)
  (when *db*
    (close-db *db*)
    (setf *db* nil)))
