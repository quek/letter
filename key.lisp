(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defvar *titles* '*titles* "memo をメンバ、更新日時をスコアにした zset")

(defvar *publics* '*publics* "公開中 memo をメンバ、公開日時をスコアにした zset")

(defun user-key (id)
  #"""user #,id""")

(defun memo-key (title)
  #"""memo #,title""")

(defun auth-token-key (token)
  #"""auth-token #,token""")

(defun tag-key (tag)
  "memo のセット"
  #"""tag #,tag""")
