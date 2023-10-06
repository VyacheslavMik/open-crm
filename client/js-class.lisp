(in-package #:client.core)

(defun _instanceof (left right)
  (if (and (/= right nil) (/= (typeof -symbol) "undefined") (getprop right (@ -symbol has-instance)))
      (chain right ((@ -symbol has-instance) left))
      (instanceof left right)))

(defun _typeof (obj)
  (if (and (= (typeof -symbol) "function")
	   (= (typeof (@ -symbol iterator)) "symbol"))
      (setf _typeof (fun _typeof (obj)
		      (typeof obj)))
      (setf _typeof (fun _typeof (obj)
		      (and obj (= (typeof -symbol) "function")
			   (= (@ obj constructor) -symbol)
			   (if (/= obj (@ -symbol prototype)) "symbol" (typeof obj))))))
  (_typeof obj))

(defun _class-call-check(instance -constructor)
  (when (not (_instanceof instance -constructor))
    (throw (new (-type-error "Cannot call a class as a function")))))

(defun _define-properties (target props)
  (loop for descriptor in props
	do (progn
	     (setf (@ descriptor enumerable) (or (@ descriptor enumerable) false))
	     (setf (@ descriptor configurable) true)
	     (when (in "value" descriptor)
	       (setf (@ descriptor writable) true))
	     (chain -object (define-property target (@ descriptor key) descriptor)))))

(defun _create-class(-constructor proto-props static-props)
  (when proto-props
    (_define-properties (@ -constructor prototype) proto-props))
  (when static-props
    (_define-properties -constructor static-props))
  -constructor)

(defun _possible-constructor-return (self call)
  (when (and call (or (= (_typeof call) "object") (= (typeof call) "function")))
    (return-from _possible-constructor-return call))
  (_assert-this-initialized self))

(defun _get-prototype-of (o)
  (setf _get-prototype-of (if (@ -object set-prototype-of)
			      (@ -object get-prototype-of)
			      (fun _get-prototype-of (o)
				(or (@ o __proto__) (chain -object (get-prototype-of o))))))
  (_get-prototype-of o))

(defun _assert-this-initialized (self)
  (when (= self undefined)
    (throw (new (-reference-error "this hasn't been initialised - super() hasn't been called"))))
  self)

(defun _inherits (sub-class super-class)
  (when (and (/= (typeof super-class) "function") (/= super-class nil))
    (throw (new (-type-error "Super expression must either be null or a function"))))

  (setf (@ sub-class prototype) (chain -object (create (and super-class (@ super-class prototype))
						       (create
							constructor (create
								     value sub-class
								     writable true
								     configurable true)))))
  (when super-class
    (_set-prototype-of sub-class super-class))
  nil)

(defun _set-prototype-of (o p)
  (setf _set-prototype-of (or (@ -object set-prototype-of)
			      (fun _set-prototype-of (o p)
				(setf (@ o __proto__) p)
				o)))
  (_set-prototype-of o p))

(eval-when (:compile-toplevel)
  (defpsmacro def-js-class (name super &body methods)
    (let* ((ctor (find 'ctor methods :key #'car))
	   (methods (remove 'ctor methods :key #'car))
	   (methods (mapcar (lambda (m)
			      (list
			       (list (car m) (symbol-to-js-string (car m)))
			       `(fun ,(car m) ,(cadr m)
				     ,@(cddr m))))
			    methods)))
      `(defvar ,name
	 ((lambda (super-class)
	    (_inherits ,name super-class)
	    (defun ,name ,(cadr ctor)
	      (_class-call-check this ,name)
	      (let ((_this (_possible-constructor-return
			    this
			    (chain (_get-prototype-of ,name) (call this ,@(cadr ctor))))))
		,@(code-replace-symbol
		   (cddr ctor)
		   #'replace-symbol-this)
		_this))
	    (_create-class ,name
			   (array
			    ,@(mapcar
			       (lambda (k)
				 `(create
				   key ,(cadar k)
				   value ,(cadr k)))
			       methods)))
	    ,name)
	  ,super)))))
