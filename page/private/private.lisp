(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; テンプレート
(defmacro with-default-template ((&key (title "letter") (login-required t))
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
               (:link :rel "stylesheet" :href "/css/bootstrap.min.css")
               (:link :rel "stylesheet" :href "/css/bootstrap-theme.min.css")
               (:link :href "/main.css" :rel "stylesheet" :type "text/css")
               (:script :src "/js/jquery-2.1.4.min.js")
               (:script :src "/js/underscore-min.js"))
             (:body
                 (:nav.navbar.navbar-inverse
                  (:div.container-fluid
                   (:div.navbar-header
                    (:a.navbar-brand :href "/" "letter"))
                   (:div.navbar-right
                    (:div.login-user (and (current-user) (email-of (current-user))))
                    (:form.form-inline :action "/search" :method "get"
                      (:input :type "search" :name "q" :value (param "q"))))))
               (:div.container-fluid
                (awhen (errors)
                  (html (:ul (loop for error in it do
                    (html (:li.text-warning error))))))
                ,@contents)
               (:script :src "/js/bootstrap.min.js")))))))

(defun markdown-editor (body)
  (html
    (:textarea#body.markdown-textarea :name "body" body)))

;; トップページ
(defaction /root (:path "/")
  (with-default-template ()
    (:p (:a.btn.btn-default :href "/new" "新しく作る"))
    (:p (:a :href "/public" "公開ページ"))
    (:ul (loop for (memo . time) in (zrang *titles* 0 nil :from-end t :with-scores t) do
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

(defun markdown-form (body &key (publicp nil) (tags ""))
  (html
    (:div.row
     (:div.col-xs-6
      (markdown-editor body)
      (:div.form-group
       (:input#tags.form-control :type "text" :name "tags" :value tags
                                 :placeholder "スペース区切りで タグ"))
      (:div.form-group
       (:label :for "public"
         (:input#public :type "checkbox" :name "public" :value 1 :checked publicp)
         "公開"))
      (:button.btn.btn-primary :type "submit" "save"))
     (:div.col-xs-6
      (markdown-preview)))))

(defaction /edit/@title ()
  (let ((memo (find-memo @title)))
    (with-default-template (:title @title)
      (:h1 @title)
      (:form :action #"""/update/#,@title""" :method "post"
        (markdown-form (body-of memo)
                       :publicp (publicp memo)
                       :tags (tags-as-string memo))))))

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
      (progn
        (create-memo :title @title
                     :body @body
                     :public @public
                     :tags @tags)
        (redirect (format nil "/show/~a" @title)))))

(defaction /update/@title (:method :post)
  (update-memo @title :body @body :public @public :tags @tags)
  (redirect (format nil "/show/~a" @title)))

(defun print-markdown (body)
  (let* ((3bmd-code-blocks:*code-blocks* t)
         (3bmd-code-blocks:*code-blocks-default-colorize* :common-lisp)
         (html (with-output-to-string (out)
                 (3bmd:parse-string-and-print-to-stream body out)))
         (html (ppcre:regex-replace-all "\\[\\[([^]]+)\\]\\]"
                                        html
                                        "<a href=\"/show/\\1\">\\1</a>"))
         (html (ppcre:regex-replace-all "(>[^<]*)(https?://[^\\s<]+)"
                                        html
                                        #"""\1<a href="\2" target="_blank">\2</a>""")))
    (write-string html *request*))
  nil)

(defaction /show/@title ()
  (let ((memo (find-memo @title)))
    (with-default-template (:title @title)
      (:h1 @title)
      (if (publicp memo)
          (html (:p.text-danger "公開"))
          (html (:p  "非公開")))
      (print-markdown (body-of memo))
      (:p (loop for tag in (tags-of memo)
                do (html (:a :href #"""/tag/#,tag""" tag)
                     " ")))
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

(defaction /tag/@tag ()
  (with-default-template (:title @tag)
    (:h1 @tag)
    (:ul
        (loop for memo in (memos-by-tag @tag)
              do (html (:li.memo-as-list
                        (:a :href #"""/show/#,(title-of memo)"""
                          (:h3 (title-of memo))
                          (:span.time (time-to-s (updated-at memo)))
                          (when (publicp memo)
                            (html (:span.public "公開"))))))))))
