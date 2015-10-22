(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defmacro with-public-template ((&key (title "letter")) &body contents)
  `(html
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
           (:div.container-fluid
            (:p (:a :href "/public" "letter"))
            ,@contents)
         (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js")))))

(defaction /public ()
  (with-public-template (:title "メモ")
    (:div.row
     (:div.col-xs-3
      (:ul (loop for (memo . time) in (zrang *publics* 0 nil :from-end t :with-scores t) do
        (html (:li.memo-as-list
               (:a :href #"""/public/#,(title-of memo)"""
                 (:h3 (title-of memo))
                 (:span.time (time-to-s time))))))))
     (:div.col-xs-9
      (awhen (car (zrang *publics* 0 0 :from-end t))
        (html (:h1 (title-of it)
                (print-markdown (body-of it)))))))))

(defaction /public/@title ()
  (let ((memo (find-memo @title)))
    (unless (publicp memo)
      (error (make-condition 'not-found-error)))
    (with-public-template (:title @title)
      (:h1 @title)
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
