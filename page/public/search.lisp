(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

(defaction /public/search ()
  (with-public-template (:title #"""#,@q""")
    (:h1 #"""「#,@q,」の検索結果""")
    (:ul
        (loop for (memo . score) in (search-search @q :with-scores t) do
          (html (:li.memo-as-list
                 (:a :href #"""/public/show/#,(title-of memo)"""
                   (:h3 (title-of memo))
                   (:span.time (time-to-s (updated-at memo)))
                   " "
                   (:span score))))))))
