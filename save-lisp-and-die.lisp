;; sbcl --load save-lisp-and-die.lisp

(ql:quickload "letter")

(defun toplevel ()
  (restart-case
      (handler-bind ((condition (lambda (c)
                                  (print c)
                                  (invoke-restart 'abort))))
        (funcall (intern "START" "LETTER"))
        (loop (sleep 1)))
    (abort (&rest args)
      (print args)
      (funcall (intern "STOP" "LETTER"))
      (print "letter:stop")
      (sb-ext:exit))))

(let ((file (merge-pathnames "bin/letter.sbcl"
                             #.(or *compile-file-truename* *load-truename*))))
  (ensure-directories-exist file)
  (sb-ext:save-lisp-and-die file :executable t :toplevel 'toplevel))
