(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *analytics-code*
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
})(window,document,'script','//www.google-analytics.com/analytics.js','ga');
ga('create', 'UA-69873049-1', 'auto');
ga('send', 'pageview');"))

(defmacro with-public-template ((&key (title "letter")) &body contents)
  `(html
     (:!doctype :html t)
     (:html :lang "ja"
       (:head (:meta :charset "UTF-8")
         (:meta :name "viewport" :content "width=device-width, initial-scale=1")
         (:title ,title)
         (:link :rel "stylesheet" :href "/css/bootstrap.min.css")
         (:link :rel "stylesheet" :href "/css/bootstrap-theme.min.css")
         (:link :href "/main.css" :rel "stylesheet" :type "text/css")
         (:script :src "/js/jquery-2.1.4.min.js")
         (:script :src "/js/underscore-min.js")
         (:script (raw ,*analytics-code*)))
       (:body
           (:div.container-fluid
            (:form.form-inline
             :style "float: right; margin-top: 5px;"
             :action "/public/search" :method "get"
             (:input.form-control :type "search" :name "q" :value (param "q") :placeholder "検索"))
            (:p (:a :href "/public" "letter"))
            ,@contents)
         (:script :src "/js/bootstrap.min.js")))))

(defaction /public ()
  (with-public-template (:title "letter")
    (:div.row
     (:div.col-xs-3
      (:ul (loop for (memo . time) in (zrang *publics* 0 nil :from-end t :with-scores t) do
        (html (:li.memo-as-list
               (:a :href #"""/public/#,(title-of memo)"""
                 (:h3 (title-of memo))
                 (:span.time (time-to-s time))))))))
     (:div.col-xs-9
      (awhen (car (zrang *publics* 0 0 :from-end t))
        (html (:h1 (title-of it))
          (:p.time (time-to-s (public-at it)))
          (print-markdown (body-of it))))))))

(defaction /public/@title ()
  (let ((memo (find-memo @title)))
    (unless (publicp memo)
      (error (make-condition 'not-found-error)))
    (with-public-template (:title @title)
      (:h1 @title)
      (:p.time (time-to-s (public-at memo)))
      (print-markdown (body-of memo))
      (:p (loop for tag in (tags-of memo)
                do (html (:a :href #"""/public/tag/#,tag""" tag)
                     " "))))))

(defaction /public/tag/@tag ()
  (with-public-template (:title @tag)
    (:h1 @tag)
    (loop for memo in (memos-by-tag @tag)
          if (publicp memo)
          do (html (:li.memo-as-list
                    (:a :href #"""/public/#,(title-of memo)"""
                      (:h3 (title-of memo))
                      (:span.time (time-to-s (updated-at memo)))))))))
