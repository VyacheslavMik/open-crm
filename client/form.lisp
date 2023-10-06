(in-package #:client.core)

(eval-when (:compile-toplevel)
  (defpsmacro update-state (this &body items)
    (let ((o     (gensym))
	  (state (gensym))
	  (items (remove-if (lambda (item)
			      (eq (car item) :on-new-state))
			    items))
	  (on-new-state (find :on-new-state items :key #'car)))
      `(chain
	,this (set-state
	       (lambda (,state)
		 (let ((,o (chain -object (assign (create) ,state))))
		   ,@(mapcar (lambda (item)
			       ;; TODO: optimise. Collect all paths and generate
			       ;; new values for unique ones.
			       `(progn
				  ,@(when (> (length (car item)) 1)
				      (mapcar (lambda (path)
						`(unless (@ ,o ,@path)
						   (setf (@ ,o ,@path) (create))))
					      (reverse
					       (maplist (lambda (l)
							  (reverse l))
							(cdr (reverse (car item)))))))
				  (setf (@ ,o ,@(car item)) ,@(cdr item))))
			     items)
		   ,(when on-new-state
		      (destructuring-bind (_ lambda-list &rest body) on-new-state
			(declare (ignore _))
			(assert (= (length lambda-list) 1))
			(let ((sym (car lambda-list)))
			  `(let ((,sym ,o))
			     ,@body))))
		   ,o))))))

  (defun make-set-state (state-field path value)
    `(chain
      this (set-state
	    (lambda (state)
	      (let ((o (chain -object
			      (assign (create)
				      (@ state ,state-field)))))
		(setf (@ o ,@path) ,value)

		(when (@ this props on-change)
		  (chain this props (on-change o)))

		(create ,state-field o))))))

  (defun update-fields (forms)
    (let (fields)
      (values
       (mapcar (lambda (form)
		 (cond
		   ((and (listp form) (eq (car form) 'form-field))
		    (let ((sym (gensym)))
		      (push (list sym form) fields)
		      (destructuring-bind (input path validators props) (cdr form)
			(let ((props (loop for (k v) on props by #'cddr
					   collect (intern (symbol-name k))
					   collect v))
			      (on-change `(event-handler (v)
					    (let (error-value)
					      (block ,sym
						,@(mapcar (lambda (v)
							    `(let ((err (,v v)))
							       (when err
								 (setf error-value err)
								 (return-from ,sym))))
							  validators))
					      ,(make-set-state 'errors (list sym) 'error-value))
					    ,(make-set-state 'form-value path 'v))))
			  `((:div class ,(if (and (listp props)
						  (member 'class props))
					     (list :form-field (cadr (member 'class props)))
					     :form-field))
			    ((,input ,@(if (listp props)
					   `(,@props
					     value (@ this state form-value ,@path)
					     error (@ this state errors ,sym)
					     validators (array ,@validators)
					     ,@(unless (find 'on-change props)
						 `(on-change ,on-change)))
					   `((progn
					       (setf (@ ,props on-change) ,on-change
						     (@ ,props value) (@ this state form-value ,@path)
						     (@ ,props error) (@ this state errors ,sym)
						     (@ ,props validators) (array ,@validators))
					       ,props))))))))))
		   ((listp form)
		    (multiple-value-bind (form new-fields) (update-fields form)
		      (setf fields (concatenate 'list fields new-fields))
		      form))
		   (t
		    form)))
	       forms)
       fields)))

  (defun make-init-value (init-value fields)
    (if init-value
	(cdr init-value)
	(let ((paths (mapcar #'caddr (mapcar #'cadr fields))))
	  (labels ((group (lists)
		     (reduce (lambda (acc l)
			       (let ((foundp (find (car l) acc :key #'car)))
				 (if foundp
				     (nconc foundp (list (cdr l)))
				     (push (if (cdr l)
					       (list (car l) (cdr l))
					       l)
					   acc))
				 acc))
			     lists :initial-value '()))
		   (build (paths)
		     (let ((paths (group paths)))
		       `(create ,@(apply
				   #'concatenate 'list
				   (mapcar (lambda (path)
					     (if (cdr path)
						 `(,(car path) ,(build (cdr path)))
						 `(,(car path) "")))
					   paths))))))
	    (list (build paths))))))

  (defpsmacro def-form (name &body body)
    (let ((body (macroexpand-all body)))
      (multiple-value-bind (render fields) (update-fields (find 'render body :key #'car))
	(let* ((init-value (make-init-value (find 'init-value body :key #'car) fields)))
	  `(def-widget ,name
	     (submit ()
	       (let ((errors (create)))
		 ,@(mapcar (lambda (field)
			     (let ((sym (car field))
				   (path (caddr (cadr field)))
				   (validators (cadddr (cadr field))))
			       `(block ,sym
				  ,@(mapcar (lambda (v)
					      `(let ((err (,v (@ this state form-value ,@path))))
						 (when err
						   (setf (@ errors ,sym) err)
						   (return-from ,sym))))
					    validators))))
			   fields)
		 (chain this (set-state (create errors errors)))
		 (when (empty-object-p errors)
		   ,@(cdr (find 'submit body :key #'car)))))

	     (ctor ()
	       (setf (@ this state) (create form-value ,@init-value
					    errors (create))))

	     (component-did-mount ()
	       (when (@ this props form-value)
		 (let ((form-value (merge-objects (@ this state form-value)
						  (@ this props form-value))))
		   (update-state this
		     ((form-value) form-value))))
	       ,@(cdr (find 'component-did-mount body :key #'car)))

	     (render ()
	       ((:form class-name (@ this props class-name))
		,@(cdr render)))))))))
