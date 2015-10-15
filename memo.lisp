(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defvar *titles* '*titles* "memo をメンバ、更新日時をスコアにした zset")
(defvar *session-user* "user" "(id-of user) を設定するセッションキー")
(defvar *cookie-auth-token* "auth" "認証トークンを設定するクッキーキー")
(defparameter *auth-token-expire-seconds* (* 60 60 24 14)
  "認証トークンの有効期間 14日")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass* memo ()
  ((title)
   (body :reader body-of :initform "" :initarg nil)
   (public :accessor publicp :initform nil)
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
(defun memo-key (title)
  #"""memo #,title""")

(defun find-memo (title &key (not-found-error-p t))
  (or (@ (memo-key title))
      (if not-found-error-p
          (error (make-condition 'not-found-error))
          nil)))

(defun user-key (id)
  #"""user #,id""")

(defun auth-token-key (token)
  #"""auth-token #,token""")

(defun current-user ()
  (or (@ (user-key (unpyo:session *session-user*)))
      (awhen (unpyo:cookie *cookie-auth-token*)
        (awhen (@ it)
          (setf (unpyo:session *session-user*) (id-of it))
          it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テンプレート
(defmacro with-default-template ((&key (title "memo") (login-required t))
                                 &body contents)
  `(progn
     (if (and ,login-required (not (current-user)))
         (redirect "/login")
         (html
           (:!doctype :html t)
           (:html :lang "ja"
             (:head (:meta :charset "UTF-8")
               (:meta :name "viewport" :content "width=device-width, initial-scale=1")
               (:title ,title)
               (:link :rel "stylesheet"
                 :href "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap.min.css")
               (:link :rel "stylesheet" :href
                 "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/css/bootstrap-theme.min.css")
               (:link :href "/main.css" :rel "stylesheet" :type "text/css")
               (:script :src "https://code.jquery.com/jquery-2.1.4.min.js")
               (:script :src "https://cdnjs.cloudflare.com/ajax/libs/underscore.js/1.8.3/underscore-min.js"))
             (:body
                 (:nav.navbar.navbar-inverse
                  (:div.container-fluid
                   (:div.navbar-header
                    (:a.navbar-brand :href "/" "memo"))
                   (:div.navbar-right
                    (:div.login-user (and (current-user) (email-of (current-user)))))))
               (:div.container-fluid
                (awhen (errors)
                  (html (:ul (loop for error in it do
                    (html (:li.text-warning error))))))
                ,@contents)
               (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js")))))))

(defun markdown-editor (body)
  (html
    (:textarea#body.markdown-textarea :name "body" body)))

;; トップページ
(defaction /root (:path "/")
  (with-default-template ()
    (:p (:a.btn.btn-default :href "/new" "新しく作る"))
    (:ul (loop for (memo . time) in (zrang *titles*  0 nil :from-end t :with-scores t) do
      (html (:li.memo-as-list
             (:a :href #"""/show/#,(title-of memo)"""
               (:h3 (title-of memo))
               (:span.time (time-to-s time))
               (when (publicp memo)
                 (html (:span.public "公開"))))))))))

(defaction /login ()
  (with-default-template (:login-required nil)
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
            (if (and (fboundp 'auth-check) (not (auth-check user)))
                (progn
                  (add-error "そのアカウントではログインできません。")
                  (redirect "/login"))
                (progn
                  (! (user-key (id-of user)) user)
                  (setf (unpyo:session *session-user*) (id-of user))
                  (let ((auth-token-key (auth-token-key (generate-token user))))
                    (! auth-token-key user)
                    (expire auth-token-key *auth-token-expire-seconds*)
                    (setf (unpyo:cookie *cookie-auth-token*
                                        :expires `(,*auth-token-expire-seconds* :sec)
                                        :http-only t)
                          auth-token-key))
                  (redirect "/"))))
          (redirect "/login"))
    (oauth2::request-token-error (e)
      (print e)
      (redirect "/login"))))

(defun markdown-preview ()
  (html
    (:div#preview-area "プレビュー")
    (js
      ($ (^ (let (($body ($ "#body")))
              (\. ($ document)
                  (on "keydown input" "#body"
                      (\. *_ (debounce (^ (\. $
                                              (get "/preview"
                                                   ({} body (\. $body (val)))
                                                   (^ (\. ($ "#preview-area")
                                                          (html _r))))))
                                       100))))
              (if (\. $body (val))
                  (\. $body
                      (trigger "keydown")))))))))

(defaction /preview ()
  (print-markdown @body))

(defun markdown-form (body &key (publicp nil))
  (html(:div.row
        (:div.col-xs-6
         (markdown-editor body)
         (:div.form-group
          (:label :for "public"
            (:input#public :type "checkbox" :name "public" :value t :checked publicp)
            "公開"))
         (:button.btn.btn-primary :type "submit" "save"))
        (:div.col-xs-6
         (markdown-preview)))))

(defaction /edit/@title ()
  (let ((memo (find-memo @title)))
    (with-default-template (:title @title)
      (:h1 @title)
      (:form :action #"""/save/#,@title""" :method "post"
        (markdown-form (body-of memo) :publicp (publicp memo))))))

(defaction /new ()
  (with-default-template (:title "新しく作る")
    (:form :action #"""/create""" :method "post"
      (:div.form-group
       (:label :for "title" "タイトル")
       (:input#title.form-control :type "text" :name "title" :value @title))
      (markdown-form @body))))

(defaction /create (:method :post)
  (if (blankp @title)
      (progn
        (add-error "タイトルを入力してください。")
        (/new))
      (let ((memo (make-instance 'memo
                                 :title @title
                                 :body @body)))
        (! (memo-key @title) memo)
        (zadd *titles* (updated-at memo) memo)
        (redirect (format nil "/show/~a" @title)))))

(defaction /save/@title (:method :post)
  (let ((memo (find-memo @title)))
    (setf (body-of memo) @body)
    (setf (publicp memo) @public)
    (setf (updated-at memo) (get-universal-time))
    (setf (updated-by memo) (current-user))
    (zadd *titles* (updated-at memo) memo)
    (redirect (format nil "/show/~a" @title))))

(defun print-markdown (body)
  (let* ((3bmd-code-blocks:*code-blocks* t)
         (3bmd-code-blocks:*code-blocks-default-colorize* :common-lisp)
         (html (with-output-to-string (out)
                 (3bmd:parse-string-and-print-to-stream body out))))
    (write-string
     (ppcre:regex-replace-all "\\[\\[([^]]+)\\]\\]"
                              html
                              "<a href=\"/show/\\1\">\\1</a>")
     *request*))
  nil)

(defaction /show/@title ()
  (let ((memo (find-memo @title)))
    (with-default-template (:title @title)
      (:h1 @title)
      (if (publicp memo)
          (html (:p.text-danger "公開"))
          (html (:p  "非公開")))
      (print-markdown (body-of memo))
      (:p (:a.btn.btn-primary :href #"""/edit/#,@title""" "編集"))
      (:p (:a.btn.btn-default :href #"""/show/#,@title,/history""" "履歴")
        " "
        (:a.btn.btn-danger :href #"""/delete/#,@title""" "削除")))))

(defaction /show/@title/history ()
  (let ((memo (find-memo @title)))
    (with-default-template (:title @title)
      (loop for h in (histroies-of memo) do
        (html (:pre (diff-of h))))
      (html (:p (:a :href #"""/show/#,@title""" "戻る"))))))

(defaction /delete/@title ()
  (let* ((key (memo-key @title))
         (memo (@ key)))
    (zrem *titles* memo)
    (del key))
  (redirect "/"))


(defvar *server*)

(defclass memo-app (application)
  ())

(defmethod call :around ((app memo-app))
  (handler-case (call-next-method)
    (not-found-error ()
      (html
        (:h1 "見つかりません。")))))

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
  (setq *server* (make-server :app (make-instance 'memo-app) :port port))
  (run *server*))

;; stop
(defun stop ()
  (unpyo:stop *server*)
  (when *db*
    (close-db *db*)
    (setf *db* nil)))
