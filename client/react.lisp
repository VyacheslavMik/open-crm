(in-package #:client.core)

(eval-when (:compile-toplevel)
  (defun fragment-p (ob)
    (eql ob :<>))

  (defun react-component (k)
    (get 'react-component k))

  (defun component-p (ob)
    (let ((s (cond
	       ((and (listp ob) (listp (car ob)))
		(caar ob))
	       ((listp ob)
		(car ob)))))
      (and s (or (html-tag-p s)
		 (fragment-p s)
		 (react-component s)))))

  (defun transform-class-prop (props)
    (labels ((function-formp (form)
	       (and (listp form)
		    (not (keywordp (car form)))
		    (symbolp (car form)))))
      (when (getf props 'class)
	(let ((class (getf props 'class)))
	  (setf (getf props 'class-name) (cond
					   ((and (listp class)
						 (every #'keywordp class))
					    `(+ ,@(apply #'concatenate
							 'list
							 (mapcar (lambda (class) (list " " class))
								 class))))

					   ((function-formp class)
					    class)

					   ((listp class)
					    `(chain (list ,@class)
						    (reduce (lambda (acc v)
							      (if (blank-stringp v)
								  acc
								  (+ acc " " v))))))

					   (t
					    class)))
	  (remf props 'class)))
      props))

  (defun transform-style-prop (props)
    (when (getf props 'style)
      (let ((style (getf props 'style)))
	(setf (getf props 'style) `(create ,@style))))
    props)

  (defun transform-props (props)
    (transform-class-prop
     (transform-style-prop
      props)))

  (defun expand-react-component (component)
    (cond
      ((component-p component)
       (let* ((el* (car component))
	      (s   (if (listp el*)
		       (car el*)
		       el*))
	      (el  (cond
		     ((html-tag-p s)
		      (string-downcase (symbol-name s)))

		     ((fragment-p s)
		      '(@ -react -fragment))

		     (t
		      (react-component s))))
	      (props (when (listp el*)
		       `(create ,@(transform-props (cdr el*)))))
	      (children (mapcar #'expand-react-component (cdr component))))
	 `(chain -react (create-element ,el ,props ,@children))))

      ((listp component)
       (mapcar #'expand-react-component component))

      (t
       component)))

  (defpsmacro react-root (component &optional (id "root"))
    `(chain -react-d-o-m
	    (render
	     ,(expand-react-component (list component nil))
	     (chain document (get-element-by-id ,id)))))

  (defpsmacro react-element (el)
    (expand-react-component el))

  (defpsmacro dynamic-element (&rest el)
    `(chain -react (create-element ,@el)))

  (defun keyword->react-name (k)
    (if (char= (elt (symbol-name k) 0) #\-)
	k
	(make-symbol (format nil "-~a" k))))

  (defun register-react-component (name)
    (setf (get 'react-component name) (keyword->react-name name)))

  (defpsmacro def-react-component (name &body body)
    (let* ((ctor (find 'ctor body :key #'car))
	   (methods (remove 'ctor body :key #'car))
	   (binds (mapcar (lambda (m)
			    `(setf (@ this ,(car m)) (chain this ,(car m) (bind this))))
			  methods))
	   (ctor (if ctor
		     (concatenate 'list ctor binds)
		     `(ctor (props) ,@binds)))
	   (body (cons ctor methods)))
      `(def-js-class ,name (@ -react -component)
	 ,@body)))

  (defun client-macrop (symbol)
    (when (gethash symbol ps::*macro-toplevel*)
      (let ((package-name (package-name (symbol-package symbol))))
	(equalp (subseq package-name 0 (position #\. package-name)) "client"))))

  (defun macroexpand-all (body)
    (cond
      ((and (listp body) (client-macrop (car body)))
       (macroexpand-all (ps::ps-macroexpand-1 body)))

      ((listp body)
       (mapcar #'macroexpand-all body))

      (t
       body)))

  (defpsmacro def-view (name lambda-list &body body)
    (register-react-component name)
    (let ((cname (keyword->react-name name))
	  (body (macroexpand-all body)))
      `(progn
	 (defun ,cname ,lambda-list
	   (react-element ,@body))
	 (setf (getprop *component-map* ,name) ,cname))))

  (defpsmacro def-widget (name &body body)
    (register-react-component name)
    (let ((cname (keyword->react-name name))
	  (body (macroexpand-all body)))
      `(progn
	 (def-react-component ,cname
	   ,@(mapcar
	       (lambda (m)
		 (if (eq (car m) 'render)
		     `(render ,(cadr m)
			      ,@(mapcar (lambda (el)
					  `(react-element ,el))
					(cddr m)))
		     m))
	       body))
	 (setf (getprop *component-map* ,name) ,cname))))

  (defpsmacro react-clone (el props)
    `(chain -react (clone-element ,el ,props))))

(defun react-children-count (el)
  (chain -react -children (count (@ el props children))))

(defvar *component-map* (create))

(defun find-react-component (name)
  (getprop *component-map* name))

(defvar *shared-values* (create))

(eval-when (:compile-toplevel)
  (defpsmacro def-shared (name)
    `(setf (@ *shared-values* ,name) (create value nil
					     subs (create)))))

(eval-when (:compile-toplevel)
  (defpsmacro shared/get (name)
    `(@ *shared-values* ,name value)))

(defun shared/run-subscriptions (name)
  (for-in (k (getprop *shared-values* name 'subs))
	  (funcall (getprop *shared-values* name 'subs k)
		   (getprop *shared-values* name 'value))))

(eval-when (:compile-toplevel)
  (defpsmacro shared/set (name val)
    `(progn
       (setf (@ *shared-values* ,name value) ,val)
       (shared/run-subscriptions ',name))))

(eval-when (:compile-toplevel)
  (defpsmacro shared/subscribe (name)
    (let ((sym (gensym)))
      `(progn
	 (let ((sub (create name ',name
			    sym ',sym)))
	   (chain this (set-state (create subs (if (and (@ this state)
							(@ this state subs))
						   (chain this state subs (push sub))
						   (array sub))))))
	 (setf (@ *shared-values* ,name subs ,sym)
	       (event-handler (v)
		 (chain this (set-state (create ,name v)))))))))

(eval-when (:compile-toplevel)
  (defpsmacro shared/unsubscribe ()
    `(dolist (sub (or (@ this state subs) #()))
       (delete (getprop *shared-values* (@ sub name) 'subs (@ sub sym))))))
