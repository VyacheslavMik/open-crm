;; to run exec this `screen -D -m sbcl --load open-crm/simple-ci.lisp`

(ql:quickload "hunchentoot")

(defvar *http-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4000))

(defvar *base-pathname* (or *load-truename* *compile-file-truename*))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (setf (hunchentoot:content-type*) "text/plain")
  (uiop:launch-program
   (namestring (merge-pathnames "run.sh" *base-pathname*))
   :directory (directory-namestring *base-pathname*))
  (format nil "ok"))

(hunchentoot:start *http-acceptor*)
