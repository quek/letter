(in-package :letter)

(defaction /main.css ()
  (setf (content-type) "text/css")
  (write-string colorize:*coloring-css* *request*)
  (css
    `((.markdown-textarea :width 100% :height 400px)
      (.login-user :color \#eee :margin 5px 10px 0 0)
      (.memo-as-list
       (h3 :display inline)
       (.time :font-size 12px :margin-left 10px)
       (.public :fort-size 12px :color \#a94442 :padding-left 3px))
      (\#preview-area \:first-child :margin-top 0))))
