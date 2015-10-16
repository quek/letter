(in-package :memo)

(named-readtables:in-readtable info.read-eval-print.double-quote:|#"|)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *titles* '*titles* "memo をメンバ、更新日時をスコアにした zset")
(defvar *publics* '*publics* "公開中 memo をメンバ、公開日時をスコアにした zset")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defclass* memo ()
  ((title)
   (body :reader body-of :initform "" :initarg nil)
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

(defmethod initialize-instance :after ((memo memo) &rest initargs &key body)
  (declare (ignore initargs))
  (setf (body-of memo) body)
  (unless (slot-boundp memo 'created-by)
    (setf (created-by memo) (updated-by memo)))
  (when (publicp memo)
    (setf (public-at memo) (get-universal-time))))

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

(defun memo-key (title)
  #"""memo #,title""")

(defun find-memo (title &key (not-found-error-p t))
  (or (@ (memo-key title))
      (if not-found-error-p
          (error (make-condition 'not-found-error))
          nil)))

(defun create-memo (&key title body public)
  (aprog1 (make-instance 'memo
                         :title title
                         :body body
                         :public public)
    (! (memo-key title) it)
    (zadd *titles* (updated-at it) it)
    (when public
      (zadd *publics* (public-at it) it))))

(defun update-memo (title &key (body nil body-supplied-p)
                     (public nil public-supplied-p))
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
        (zrem *publics* it))))

#+nil
(describe (aprog1 (make-instance 'memo)
            (setf (publicp it) "1")))
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

(defun user-key (id)
  #"""user #,id""")

(defun auth-token-key (token)
  #"""auth-token #,token""")
