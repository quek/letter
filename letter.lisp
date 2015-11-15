(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defvar *session-user* "user" "(id-of user) を設定するセッションキー")
(defvar *cookie-auth-token* "auth" "認証トークンを設定するクッキーキー")
(defparameter *auth-token-expire-seconds* (* 60 60 24 14)
  "認証トークンの有効期間 14日")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun current-user ()
  (or (@ (user-key (unpyo:session *session-user*)))
      (awhen (unpyo:cookie *cookie-auth-token*)
        (awhen (@ it)
          (setf (unpyo:session *session-user*) (id-of it))
          it))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *server*)

(defclass memo-app (static-application application)
  ())

(defun static-mappings ()
  `(("/" ,(merge-pathnames "public/" *default-directory*))))

(defmethod call :around ((app memo-app))
  (handler-case (call-next-method)
    (not-found-error ()
      (404-not-found app))))

(defmethod 404-not-found ((app memo-app))
  (html (:h1 "見つかりません。")))

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
  ;; Unpyo
  (setq *server*
        (make-server :app (make-instance 'memo-app
                                         :static-mappings (static-mappings))
                     :port port))
  (run *server*))

;; stop
(defun stop ()
  (unpyo:stop *server*)
  (when *db*
    (close-db *db*)
    (setf *db* nil)))
