(in-package :memo)

(defaction /main.css ()
  (setf (content-type) "text/css")
  (write-string colorize:*coloring-css* *html-output*)
  (css
    `((.markdown-textarea :width 100% :height 400px)
      (.login-user :color \#eee :margin 5px 10px 0 0)
      (.memo-as-list
       (h3 :display inline)
       (.time :font-size 12px :margin-left 10px))
      (\#preview-area \:first-child :margin-top 0))))
