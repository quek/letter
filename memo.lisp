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
    (:p (:a :href "/new" "新しく作る"))
    (:ul (iterate ((title (scan (zrang "titles" 0 nil :from-end t))))
           (html (:li (:a :href #"""/show/#,title""" title)))))))

(defaction /main.css ()
  (setf (content-type) "text/css")
  (write-string
   (cl-css:css `((body :color \#333)
                 (textarea.edit :width 80% :height 400px)))
   *html-output*))

(defun doc-key (title)
  (format nil "doc ~a" title))

(defaction /edit/@title ()
  (let ((doc (@ (doc-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (:form :action #"""/save/#,@title""" :method "post"
        (:textarea.edit :name "doc" doc)
        (:p (:input :type "submit" :value "save"))))))

(defaction /new ()
  (with-defalut-template (:title "新しく作る")
    (:form :action #"""/create""" :method "post"
      (:p (:input :type "text" :name "title"))
      (:p (:textarea.edit :name "doc" ""))
      (:p (:input :type "submit" :value "save")))))

(macrolet ((body ()
             `(unpyo::with-@param
                (! (doc-key @title) @doc)
                (zadd "titles" (get-universal-time) @title)
                (redirect (format nil "/show/~a" @title)))))
  (defaction /create (:method :post)
    (body))
  (defaction /save/@title (:method :post)
    (body)))

(defaction /show/@title ()
  (let ((doc (@ (doc-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (:pre doc)
      (:p (:a :href #"""/edit/#,@title""" "編集"))
      (:p (:a :href #"""/delete/#,@title""" "削除")))))

(defaction /delete/@title ()
  (del (doc-key @title))
  (zrem "titles" @title)
  (redirect "/"))

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
