(in-package #:server.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun extend-symbol (control-string sym)
    (make-symbol
     (string-upcase
      (with-output-to-string (s)
	(format s control-string sym))))))

(defun row->json (row)
  (let ((o (cl-json:decode-json-from-string (cadr row))))
    (push (cons :id (car row)) o)
    o))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *entity-classes* (make-hash-table)))

(defmacro def-entity-class (name &key (id-type 'uuid))
  (setf (gethash name *entity-classes*) `((:id-type . ,id-type)))
  `(progn
     (setf (gethash ',name *entity-classes*) '((:id-type . ,id-type)))
     (defclass ,name ()
       ((id :col-type ,(ecase id-type
			 (uuid 'string)
			 (int 'integer))
	    :initarg :id
	    :reader id
	    :accessor id)
	(entity :col-type jsonb :initarg :entity :reader entity :accessor entity))
       (:metaclass postmodern:dao-class)
       (:keys id))))

(defmacro init-entity (name db)
  (let ((init-fn (extend-symbol "init-~s" name)))
    `(progn
       (defun ,init-fn ()
	 (unless (query (:select t
			 :from :information_schema.tables
			 :where (:= :table_name ,(if (eq name 'user)
						     "user"
						     (s-sql:to-sql-name name)))))
	   ,(ecase (get-v (gethash name *entity-classes*) :id-type)
	      (uuid ())
	      (int `(query ,(format nil "create sequence ~(~a~)_seq start with 1" name))))
	   (execute (postmodern:dao-table-definition ',name))))
       ,(ecase db
	  (system
	   `(push #',init-fn *system-tables*))

	  (hotel
	   `(push #',init-fn *hotel-tables*))))))

(defun to-json-name (s)
  (etypecase s
    (symbol (funcall cl-json:*lisp-identifier-name-to-json* (symbol-name s)))
    (string s)))

(defun compile-where-clauses (clauses)
  (ecase (car clauses)
    (:and
     `(:and ,(mapcar #'compile-where-clauses (cdr clauses))))

    (:or
     `(:or ,(mapcar #'compile-where-clauses (cdr clauses))))

    (:=
     (destructuring-bind (path value) (cdr clauses)
       (let ((path (format nil "{~{~A~^,~}}"
			   (mapcar #'to-json-name
				   (if (listp path) path (list path))))))
	 `(:= (:#>> :entity ,path) ,value))))))

(defun search-entity (name where-clauses &key (as 'vector))
  (let* ((where (compile-where-clauses where-clauses))
	 (res (query (s-sql:sql-compile
		      `(:select :* :from ,name :where ,where)))))
    (coerce
     (map 'vector #'row->json res) as)))

(defun create-entity (name body &key (before #'identity))
  (multiple-value-bind (entity interruptp) (funcall before body)
    (or interruptp
	(let ((id (ecase (get-v (gethash name *entity-classes*) :id-type)
		    (uuid (make-uuid))
		    (int (caar (query (:select (:nextval (format nil "~(~a~)_seq" name)))))))))
	  (let* ((o (make-dao name
			      :id id
			      :entity (cl-json:encode-json-to-string entity)))
		 (row (list id (entity o))))
	    (row->json row))))))

(defun read-entity (name &key (id nil id-supplied-p) (after #'identity) (as 'vector))
  (when id-supplied-p
    (assert id))
  (let ((res (if id
		 (query (:select :* :from name :where (:= :id id)))
		 (query (:select :* :from name)))))
    (coerce
     (map 'vector (lambda (row)
		    (let ((o (row->json row)))
		      (funcall after o)))
	  res)
     as)))

(defun update-entity (name id body &key (before #'identity))
  (multiple-value-bind (entity interruptp) (funcall before body)
    (let ((entity (remove :id entity :key #'car)))
      (or interruptp
	  (let ((row (car
		      (query (:update name
			      :set :entity (cl-json:encode-json-to-string entity)
				   :where (:= :id id)
				   :returning :*)))))
	    (if row
		(row->json row)
		"not-found"))))))

(defun delete-entity (name &key id (as 'vector))
  (let ((rows (if id
		  (query (:delete-from name :where (:= :id id) :returning :*))
		  (query (:delete-from name :returning :*)))))
    (if (and id (not rows))
	"not-found"
	(coerce (map 'vector #'row->json rows) as))))

(defmacro def-entity-routes (name &key before-update before-create process-get roles)
  (let ((params `(:roles ,@(or roles (list :admin)))))
    `(progn
       (defroute (:get ,(format nil "/~(~a~)" name)) (,params)
	 (read-entity ',name ,@(when process-get
				 (list :after `(lambda (o)
						 ,process-get)))))
       
       (defroute (:get ,(format nil "/~(~a~)/:id" name)) (,params)
	 (read-entity ',name
		      :id (get-v params :id)
		      ,@(when process-get
			  (list :after `(lambda (o)
					  ,process-get)))))

       (defroute (:post ,(format nil "/~(~a~)" name)) (,params)
	 (let ((id (gensym)))
	   (declare (ignorable id))
	   (create-entity ',name body ,@(when before-create
					  (list :before `(lambda (entity)
							   (let (interruptp)
							     ,before-create
							     (or interruptp entity))))))))

       (defroute (:post ,(format nil "/~(~a~)/:id" name)) (,params)
	 (let ((id (get-v params :id)))
	   (update-entity ',name id body ,@(when before-update
					     (list :before `(lambda (entity)
							      (let (interruptp)
								,before-update
								(or interruptp entity))))))))

       (defroute (:delete ,(format nil "/~(~a~)/:id" name)) (,params)
	 (let ((id (get-v params :id)))
	   (delete-entity ',name :id id))))))

(defmacro def-entity (name &key before-update before-create process-get db roles)
  `(progn
     (def-entity-class ,name)
     (init-entity ,name ,db)
     (def-entity-routes ,name
	 ,@(when roles
	     (list :roles roles))
       ,@(when before-update
	   (list :before-update before-update))
       ,@(when before-create
	   (list :before-create before-create))
       ,@(when process-get
	   (list :process-get process-get)))))

(defun find-user (login)
  (car (search-entity 'user `(:= :login ,login) :as 'list)))

(defun get-entity (name id)
  (car (read-entity name :id id :as 'list)))

(defun get-user (id)
  (car (read-entity 'user :id id :as 'list)))

;; FIXME: very fragile macro. It is needed to be rewritten.
(defmacro user-check ()
  `(let ((user (find-user (get-v entity :login))))
     (if (and user (string/= (get-v user :id) id))
	 (progn
	   (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	   (setf interruptp "user-already-exists"))
	 (setf (get-v entity :password) (hash-password (get-v entity :password))))))

(def-entity category       :db hotel)
(def-entity service        :db hotel)
(def-entity settings       :db hotel)
(def-entity offer          :db hotel)
(def-entity booking        :db hotel)
(def-entity booking-source :db hotel)
(def-entity transfer       :db hotel)
(def-entity hotel-room     :db hotel)
(def-entity task           :db hotel)

(def-entity user
  ;; FIXME: Make something like (:get :after fn)
  :process-get (setf o (remove :password o :key #'car))
  :before-update (user-check)
  :before-create (user-check)
  :db system
  :roles (root))

(def-entity hotel
  :db system
  :roles (root))

(def-entity feedback
  :db system
  :roles (root))

(defun init-admin ()
  (unless (query (:select :id
		  :from :user
		  :where (:= :id "root")))
    (make-dao 'user
	      :id "root"
	      :entity (cl-json:encode-json-to-string
		       (list (cons :login "root")
			     (cons :password (hash-password "open-crm"))
			     (cons :role "root"))))))
