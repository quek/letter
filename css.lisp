(in-package :memo)

(defaction /main.css ()
  (setf (content-type) "text/css")
  (write-string colorize:*coloring-css* *html-output*)
  (write-string
   (cl-css:css
     `((.markdown-textarea width 100% height 400px)
       (.login-user color \#eee margin "5px 10px 0 0")
       (".memo-as-list h3" display inline)
       (".memo-as-list .time" font-size 12px margin-left 10px)))
   *html-output*))
