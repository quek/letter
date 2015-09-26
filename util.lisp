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
