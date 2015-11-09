(in-package  :letter)

(defclass search-mixin ()
  ())


(defgeneric search-fields (self))
(defgeneric search-index (self))

(defmethod search-fields (self)
  nil)

(defmethod search-index (self)
  (flet ((delete-old ()
           (loop for word in (hget *word-hash-key* self)
                 do (zrem (search-key word) self)))
         (str ()
           (with-output-to-string (out)
             (loop for i in (search-fields self)
                   do (awhen (cond ((symbolp i)
                                    (slot-value self i))
                                   ((functionp i)
                                    (funcall i self)))
                        (princ it out)
                        (write-char #\space out)))))
         (normalize (str)
           (ppcre:regex-replace-all "[。、,.\\s]" str ""))
         (add (word)
           (zinc (search-key word) self)))
    (delete-old)
    (let ((words (loop with str = (normalize (str))
                       for i from 0 below (1- (length str))
                       for s = (subseq str i (+ i 2))
                       do (add s)
                       collect s)))
      (hset *word-hash-key* self (remove-duplicates words :test #'equal)))))


#+nil
(search-index (car (lepis:zrang *titles* 5 5)))
