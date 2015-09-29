(in-package :memo)

(defun lines (string)
  (ppcre:split "\\r?\\n" string))

(defun time-to-s (time)
  (local-time:format-timestring
   nil
   (local-time:universal-to-timestamp time)
   :format `(:year "/" :month "/" :day " " :hour ":" :min ":" :sec)))

(defun blankp (thing)
  (or (null thing)
      (equal "" thing)))

(defvar *errors* '*errors* "エラーメッセージ")

(defun add-error (error)
  (push error (session *errors*)))

(defun errors ()
  (prog1 (session *errors*)
    (rem-session *errors*)))

(defmacro js (&body body)
  (labels ((f (x)
             (if (atom x)
                 (cond ((eq x '\.)
                        'ps:chain)
                       ((eq x '{})
                        'ps:create)
                       ((eq x 'let)
                        'ps:let)
                       (t x))
                 (cons (f (car x)) (f (cdr x))))))
    `(html
       (:script
           (raw
            (ps:ps ,@(f body)))))))

(ps:import-macros-from-lisp '^)
