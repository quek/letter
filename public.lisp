(in-package :memo)

(defaction /public/@title ()
  (let ((memo (find-memo @title)))
    (unless (publicp memo)
      (error (make-condition 'not-found-error)))
    (with-default-template (:title @title :login-required nil)
      (html (:h1 @title)
        (print-markdown (body-of memo))))))
