(in-package :letter)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *user-attributes* '(:id
                                  :email
                                  :name
                                  :link
                                  :picture))
(defclass* user ()
  (id
   email
   name
   link
   picture))

(defmethod lepis.util:value< ((a user) (b user))
  (string< (id-of a) (id-of b)))

(defmethod lepis.util:value= ((a user) (b user))
  (string= (id-of a) (id-of b)))

(defmethod generate-token ((user user))
  (let ((*print-base* 32))
    (prin1-to-string
     (ironclad:octets-to-integer
      (ironclad:digest-sequence
       :sha256
       (ironclad:integer-to-octets
        (logxor (ironclad:octets-to-integer (babel:string-to-octets (id-of user)))
                (get-universal-time)
                (random #xffffffff))))))))
;; (generate-token (make-instance 'user :id "1234"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass* memo ()
  ((title)
   (body :reader body-of :initform "" :initarg nil)
   (tags :initform ())
   (public :reader publicp :initform nil)
   (public-at :initform nil :accessor public-at)
   (created-at :accessor created-at :initform (get-universal-time))
   (created-by :accessor created-by :initform (current-user))
   (updated-at :accessor updated-at :initform (get-universal-time))
   (updated-by :accessor updated-by :initform (current-user))
   (histroies :initform nil)))

(defmethod lepis.util:value< ((a memo) (b memo))
  (string< (title-of a) (title-of b)))

(defmethod lepis.util:value= ((a memo) (b memo))
  (string= (title-of a) (title-of b)))

(defmethod print-object ((memo memo) stream)
  (print-unreadable-object (memo stream :type t :identity t)
    (princ (title-of memo) stream)))

(defclass* history ()
  (diff
   (updated-at :accessor updated-at)
   (updated-by :accessor updated-by)))

(defmethod initialize-instance :after ((memo memo) &rest initargs &key body tags)
  (declare (ignore initargs))
  (setf (body-of memo) body)
  (unless (slot-boundp memo 'created-by)
    (setf (created-by memo) (updated-by memo)))
  (when (publicp memo)
    (setf (public-at memo) (get-universal-time)))
  (let ((tags (normalize-tag tags)))
    (setf (slot-value memo 'tags) tags)
    (loop for tag in tags
          do (sadd (tag-key tag) memo))))

(defmethod (setf body-of) (new (memo memo))
  (let* ((old (slot-value memo 'body))
         (diff (diff:generate-seq-diff 'diff:unified-diff (lines old) (lines new)))
         (histroy (make-instance 'history
                                 :diff (with-output-to-string (s)
                                         (diff:render-diff diff s))
                                 :updated-at (updated-at memo)
                                 :updated-by (updated-by memo))))
    (push histroy (histroies-of memo))
    (setf (slot-value memo 'body) new)))

(defmethod (setf publicp) (value (memo memo))
  (setf (slot-value memo 'public) (not (blankp value))))

(defmethod (setf publicp) :before (value (memo memo))
  (let ((value (not (blankp value))))
    (cond ((and  value (or (not (publicp memo))
                           (not (slot-boundp memo 'public-at))))
           (setf (public-at memo) (get-universal-time)))
          ((and (publicp memo) (not value))
           (setf (public-at memo) nil)))))

(defmethod (setf tags-of) :around (tags (memo memo))
  (let* ((tags (normalize-tag tags))
         (before-tags (tags-of memo))
         (add (set-difference tags before-tags :test #'equal))
         (remove (set-difference before-tags tags :test #'equal)))
    (call-next-method tags memo)
    (loop for tag in add
          do (sadd (tag-key tag) memo))
    (loop for tag in remove
          do (srem (tag-key tag) memo))))

(defmethod tags-as-string ((memo memo))
  (format nil "~{~a~^ ~}" (tags-of memo)))

(defun find-memo (title &key (not-found-error-p t))
  (or (@ (memo-key title))
      (if not-found-error-p
          (error (make-condition 'not-found-error))
          nil)))

(defun memos-by-tag (tag)
  (smembers (tag-key tag)))

(defun create-memo (&key title body public tags)
  (aprog1 (make-instance 'memo
                         :title title
                         :body body
                         :public public
                         :tags tags)
    (! (memo-key title) it)
    (zadd *titles* (updated-at it) it)
    (when public
      (zadd *publics* (public-at it) it))))

(defun update-memo (title &key (body nil body-supplied-p)
                     (public nil public-supplied-p)
                     (tags "" tags-supplied-p))
  (aprog1 (find-memo title)
    (when body-supplied-p
      (setf (body-of it) body))
    (when public-supplied-p
      (setf (publicp it) public))
    (setf (updated-at it) (get-universal-time))
    (setf (updated-by it) (current-user))
    (zadd *titles* (updated-at it) it)
    (if (publicp it)
        (zadd *publics* (public-at it) it)
        (zrem *publics* it))
    (when tags-supplied-p
      (setf (tags-of it) tags))))

#+nil
(describe (aprog1 (make-instance 'memo)
            (setf (publicp it) "1")))

(defgeneric normalize-tag (tags)
  (:method (tags)
    tags)
  (:method ((tags string))
    (ppcre:split "\\s+" tags)))
