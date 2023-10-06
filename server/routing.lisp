(in-package #:server.core)

(defvar *routes* nil)

(defun normalize-part (part)
  (if (char= (elt part 0) #\:)
      (with-input-from-string (in part)
	(read in))
      part))

(defun parse-uri (uri)
  (cons
   "/"
   (mapcar #'normalize-part
	   (cl-ppcre:split "/" (subseq uri 1)))))

(defun param-existp (level part)
  (position-if
   (lambda (param)
     (and (keywordp param)
	  (not (eql param part))))
     (cdr (assoc :children level))
     :key #'car))

(defun push-route (method uri params)
  (unless (assoc method *routes*)
    (push (cons method (list (list :children))) *routes*))

  (loop with level = (cdr (assoc method *routes*))
	for part in (parse-uri uri)
	for next = (cdr (assoc part (cdr (assoc :children level))
			       :test (if (keywordp part) #'eql #'string=)))

	when (and (keywordp part) (param-existp level part))
	     do (error "Must be only one param in level")

	if next
	  do (setf level next)
	else
	  do (let ((new (list (list part (list :params) (list :children)))))
	       (nconc (assoc :children level) new)
	       (setf level (cdar new)))

	finally (setf (cdr (assoc :params level)) params)))

(defun find-route (method uri)
  (let ((parts (cons "/" (cl-ppcre:split "/" (subseq uri 1)))))
    (loop with level = (cdr (assoc method *routes*))
	  with params = ()
	  for part in parts
	  for next = (cdr (assoc part (cdr (assoc :children level))
				 :test (if (keywordp part) #'eql #'string=)))

	  if next
	    do (setf level next)
	  else
	    do (let ((param (find-if #'keywordp (cdr (assoc :children level)) :key #'car)))
		 (if param
		     (progn
		       (push (cons (car param) part) params)
		       (setf level (cdr param)))
		     (return nil)))

	  until (eql (cdr (assoc :type (cdr (assoc :params level)))) :static-folder)

	  finally (return
		    (list
		     (cons :route (cdr (assoc :params level)))
		     (cons :params params))))))

(defmacro defroute ((method uri) params &body body)
  (let ((folder (cdr (assoc :folder params)))
	(file   (cdr (assoc :file params))))
    (cond
      (folder
       `(progn
	  (push (hunchentoot:create-folder-dispatcher-and-handler ,uri ,folder) hunchentoot:*dispatch-table*)
	  (push-route ,method ,uri ',(append params (list (cons :type :static-folder))))))
      (file
       (let ((f (gensym)))
	 `(progn
	    (defun ,f () (hunchentoot:handle-static-file ,file))
	    (push-route ,method ,uri ',(append params (list (cons :type :static-file)
							    (cons :function f)))))))
      (t
       (let ((f (gensym)))
	 `(progn
	    (defun ,f (route params body aux)
	      (declare (ignorable route params body aux))
	      ,@body)
	    (push-route ,method ,uri ',(append params (list (cons :function f))))))))))
