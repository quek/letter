(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defaction /search ()
  (with-default-template (:title #"""#,@q""")
    (:h1 #"""「#,@q,」の検索結果""")
    (:ul (loop for (memo . score) in (search-search @q :with-scores t :only-public-p nil) do
      (html (:li.memo-as-list
             (:a :href #"""/show/#,(title-of memo)"""
               (:h3 (title-of memo))
               (:span.time (time-to-s (updated-at memo)))
               " "
               (:span score)
               (when (publicp memo)
                 (html (:span.public "公開"))))))))))
