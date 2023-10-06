(in-package #:server.core)

(defparameter *config* nil)
(defparameter *config-path* #p"~/open-crm.cldn")

(defun config/load ()
  (with-open-file (in *config-path*)
    (with-standard-io-syntax
      (let ((*read-eval* nil))
	(setf *config* (read in))))))

(defun config/get (&rest names)
  (loop for name in names
	for res = (getf *config* name)
	  then (getf res name)
	while res
	finally (return res)))
