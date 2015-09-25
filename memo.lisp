(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defvar *session-user* "user")

(defvar *titles* '*titles*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass* memo ()
  ((title)
   (body :reader body-of :initform "" :initarg nil)
   (created-at :accessor created-at :initform (get-universal-time))
   (created-by :accessor created-by :initform (current-user))
   (updated-at :accessor updated-at :initform (get-universal-time))
   (updated-by :accessor updated-by :initform (current-user))
   (histroies :initform nil)))

(defmethod lepis.util:value< ((a memo) (b memo))
  (string< (title-of a) (title-of b)))

(defmethod lepis.util:value= ((a memo) (b memo))
  (string= (title-of a) (title-of b)))

(defmethod print-object ((memo memo) stream)
  (print-unreadable-object (memo stream :type t :identity t)
    (princ (title-of memo) stream)))

(defclass* history ()
  (diff
   (updated-at :accessor updated-at)
   (updated-by :accessor updated-by)))

(defmethod initialize-instance :after ((memo memo) &rest initargs &key body)
  (declare (ignore initargs))
  (setf (body-of memo) body)
  (unless (slot-boundp memo 'created-by)
    (setf (created-by memo) (updated-by memo))))

(defmethod (setf body-of) (new (memo memo))
  (let* ((old (slot-value memo 'body))
         (diff (diff:generate-seq-diff 'diff:unified-diff (lines old) (lines new)))
         (histroy (make-instance 'history
                                 :diff (with-output-to-string (s)
                                         (diff:render-diff diff s))
                                 :updated-at (updated-at memo)
                                 :updated-by (updated-by memo))))
    (push histroy (histroies-of memo))
    (setf (slot-value memo 'body) new)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *user-attributes* '(:id
                                  :email
                                  :name
                                  :link
                                  :picture))
(defclass* user ()
  (id
   email
   name
   link
   picture))

(defmethod lepis.util:value< ((a user) (b user))
  (string< (id-of a) (id-of b)))

(defmethod lepis.util:value= ((a user) (b user))
  (string= (id-of a) (id-of b)))

(defmethod generate-token ((user user))
  (let ((*print-base* 32))
    (prin1-to-string
     (ironclad:octets-to-integer
      (ironclad:digest-sequence
       :sha256
       (ironclad:integer-to-octets
        (logxor (ironclad:octets-to-integer (babel:string-to-octets (id-of user)))
                (get-universal-time)
                (random #xffffffff))))))))
;; (generate-token (make-instance 'user :id "1234"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lines (string)
  (ppcre:split "\\r?\\n" string))

(defun memo-key (title)
  #"""user #,title""")

(defun user-key (id)
  #"""user #,id""")

(defun current-user ()
  (@ (user-key (unpyo:session *session-user*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro js (&body body)
  `(html
     (:script
         (raw
          (ps:ps ,@body)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テンプレート
(defmacro with-defalut-template ((&key (title "memo") (login-required t))
                                 &body contents)
  `(progn
     (if (and ,login-required (not (current-user)))
         (redirect "/login")
         (html
           (:!doctype :html t)
           (:html
             :lang "ja"
             (:head (:meta :charset "UTF-8")
               (:title ,title)
               (:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
               (:link :href "/main.css" :rel "stylesheet" :type "text/css"))
             (:body
                 (:div.menu (:div (:a :href "/" "トップ"))
                            (:div (and (current-user) (email-of (current-user)))))
               ,@contents))))))

(defaction /main.css ()
  (setf (content-type) "text/css")
  (write-string colorize:*coloring-css* *html-output*)
  (write-string
   (cl-css:css
     `((body :color \#333)
       (.menu float right)
       (".menu div" float left margin-left 10px)
       (.markdown-textarea width 90% height 400px)))
   *html-output*))

(defun markdown-editor (body)
  (html
    (:textarea.markdown-textarea :name "body" body)))

;; トップページ
(defaction /root (:path "/")
  (with-defalut-template ()
    (:h1 "メモ")
    (:p (:a :href "/new" "新しく作る"))
    (:ul (iterate ((memo (scan (zrang *titles*  0 nil :from-end t))))
           (html (:li (:a :href #"""/show/#,(title-of memo)""" (title-of memo))))))))

(defaction /login ()
  (with-defalut-template (:login-required nil)
    (:a :href #"""https://accounts.google.com/o/oauth2/auth?response_type=code&client_id=#,*oauth-client-id*,&redirect_uri=#,*oauth-callback-url*,&scope=https://www.googleapis.com/auth/userinfo.email"""
      "Google アカウントでログイン")))

(defaction /oauth2callback ()
  (handler-case
      (if @code
          (let* ((token (oauth2:request-token
                         "https://www.googleapis.com/oauth2/v3/token"
                         @code
                         :method :post
                         :redirect-uri *oauth-callback-url*
                         :other `(("client_id" . ,*oauth-client-id*)
                                  ("client_secret" . ,*oauth-client-secret*))))
                 (userinfo (with-input-from-string
                               (stream
                                (map 'string 'code-char
                                  (oauth2:request-resource
                                   "https://www.googleapis.com/oauth2/v2/userinfo"
                                   token)))
                             (json:decode-json stream)))
                 (user (apply #'make-instance 'user
                              (mapcan (^ (list _key (cdr (assoc _key userinfo))))
                                      *user-attributes*))))
            (! (user-key (id-of user)) user)
            (setf (unpyo:session *session-user*) (id-of user))
            (redirect "/"))
          (redirect "/login"))
    (oauth2::request-token-error (e)
      (print e)
      (redirect "/login"))))

(defun markdown-preview ()
  (html
    (:div#preview-area)
    (js
      ($ (lambda ()
           (ps:chain
            ($ "#preview")
            (click (lambda (e)
                     (ps:chain e (prevent-default))
                     (ps:chain
                      $
                      (get "/preview"
                           (ps:create body (ps:chain ($ "textarea[name=body]") (val)))
                           (lambda (r)
                             (ps:chain ($ "#preview-area")
                                       (html r)))))))))))))

(defaction /edit/@title ()
  (let ((memo (@ (memo-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (:form :action #"""/save/#,@title""" :method "post"
        (:p (:input :type "submit" :value "save")
          (:a#preview :href "#" "preview"))
        (:p (markdown-editor (body-of memo))))
      (markdown-preview))))

(defaction /preview ()
  (print-markdown @body))

(defaction /new ()
  (with-defalut-template (:title "新しく作る")
    (:form :action #"""/create""" :method "post"
      (:p (:input :type "submit" :value "save")
        (:a#preview :href "#" "preview"))
      (:p (:input :type "text" :name "title"))
      (:p (markdown-editor "")))
    (markdown-preview)))

(defaction /create (:method :post)
  (let ((memo (make-instance 'memo
                             :title @title
                             :body @body)))
    (! (memo-key @title) memo)
    (zadd *titles* (updated-at memo) memo)
    (redirect (format nil "/show/~a" @title))))

(defaction /save/@title (:method :post)
  (let ((memo (@ (memo-key @title))))
    (setf (body-of memo) @body)
    (setf (updated-at memo) (get-universal-time))
    (setf (updated-by memo) (current-user))
    (zadd *titles* (updated-at memo) memo)
    (redirect (format nil "/show/~a" @title))))

(defun print-markdown (body)
  (let ((3bmd-code-blocks:*code-blocks* t)
        (3bmd-code-blocks:*code-blocks-default-colorize* :common-lisp))
    (3bmd:parse-string-and-print-to-stream body *html-output*)))

(defaction /show/@title ()
  (let ((memo (@ (memo-key @title))))
    (with-defalut-template (:title @title)
      (:h1 @title)
      (print-markdown (body-of memo))
      (:p (:a :href #"""/edit/#,@title""" "編集"))
      (:p (:a :href #"""/delete/#,@title""" "削除")))))

(defaction /delete/@title ()
  (let ((memo (@ (memo-key @title))))
    (del memo)
    (zrem *titles* memo))
  (redirect "/"))


(defvar *server*)

(defclass memo-app (application)
  ())

(defmethod call :around ((app memo-app))
  (with-db ((merge-pathnames "lepis/" *default-directory*))
    (call-next-method)))

(defparameter *oauth-secret-file* (merge-pathnames "google-oauth.lisp" *default-directory*))
(defvar *oauth-client-id* nil)
(defvar *oauth-client-secret* nil)
(defvar *oauth-callback-url* nil)
(defun load-google-oauth ()
  (with-open-file (in *oauth-secret-file*)
    (let ((x (read in)))
      (setf *oauth-client-id* (getf x :client-id)
            *oauth-client-secret* (getf x :client-secret)
            *oauth-callback-url* (getf x :callback-url)))))

;; start
(defun start (&key (port *http-port*))
  (format t "http://localhost:~d~%" port)
  (load-google-oauth)
  (unless *db*
    (setf *db* (open-db (merge-pathnames "lepis/" *default-directory*))))
  ;; html
  (setf info.read-eval-print.html:*html-pprint* nil)
  ;; Unpyo
  (setf *invoke-debugger-p* t)
  (setq *server* (make-server :app (make-instance 'memo-app) :port port))
  (run *server*))

;; stop
(defun stop ()
  (unpyo:stop *server*)
  (when *db*
    (close-db *db*)
    (setf *db* nil)))
