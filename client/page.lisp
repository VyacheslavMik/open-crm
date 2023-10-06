(in-package #:client.core)

(defvar *pages* (create))

(eval-when (:compile-toplevel)
  (defpsmacro def-page (name &body body)
    `(progn
       (setf (getprop *pages* ,name) ,name)
       (def-widget ,name ,@body))))

(defun page-exists-p (page)
  (getprop *pages* page))

(def-page :not-found-page
  (render ()
    (:div
     (:div "Not found"))))

(defun goto-page (page &optional props)
  (let* ((arr (loop for k in (object-keys props)
		    collect (+ k "=" (getprop props k))))
	 (q (str (if *hotel-id*
		     (str "?hotel-id="
			  *hotel-id*
			  (if (emptyp arr)
			      ""
			      "&"))
		     (if (emptyp arr)
			 ""
			 "?"))
		 (if (emptyp arr)
		     ""
		     (join arr "&")))))
    (if (= (@ window location hash) (+ "#" page q))
	(show-page (random))
	(setf (@ window location hash) (+ page q)))))

(defun goto-app ()
  (setf (@ window location pathname) "/app"))

(defun show-page (&optional seed)
  (when *show-page*
    (let* ((hash (@ window location hash))
	   (hash (chain hash (split "?")))
	   (page (chain (aref hash 0) (substring 1)))
	   (q (aref hash 1))
	   (q (if q
		  (loop
		    with res = (create)
		    for pair in (chain (aref hash 1) (split "&"))
		    do (let ((pair (chain pair (split "="))))
			 (setf (getprop res (aref pair 0)) (aref pair 1)))
		    finally (return res))
		  (create))))
      (when seed
	(setf (getprop q "seed") seed))

      (when (/= (getprop q "hotel-id") *hotel-id*)
	(setf *hotel-id* (getprop q "hotel-id")))

      (cond
	((not (@ window location hash))
	 (goto-page :main))

	((page-exists-p page)
	 (funcall *show-page* page q))

	(t
	 (funcall *show-page* :not-found-page))))))

(setf (@ window onhashchange) (event-handler (e) (show-page)))

;; This is debug function
(defun reload-current-page ()
  (show-page (chain -math (random))))
