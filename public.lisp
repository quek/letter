(in-package :memo)

(defmacro with-public-template ((&key (title "memo")) &body contents)
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
            ,@contents)
         (:script :src "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.5/js/bootstrap.min.js")))))

(defaction /public ()
  (with-public-template (:title "メモ")
    (:div.row
     (:div.col-xs-3
      (:h2 (:a :href "/public" "memo"))
      (:ul (loop for (memo . time) in (zrang *publics* 0 nil :from-end t :with-scores t) do
        (html (:li.memo-as-list
               (:a :href #"""/public/#,(title-of memo)"""
                 (:h3 (title-of memo))
                 (:span.time (time-to-s time))))))))
     (:div.col-xs-9
      (let ((memo (car (zrang *publics* 0 0 :from-end t))))
        (html (:h1 (title-of memo)
                (print-markdown (body-of memo)))))))))

(defaction /public/@title ()
  (let ((memo (find-memo @title)))
    (unless (publicp memo)
      (error (make-condition 'not-found-error)))
    (with-public-template (:title @title)
      (html
        (:h2 (:a :href "/public" "memo"))
        (:h1 @title)
        (print-markdown (body-of memo))))))
