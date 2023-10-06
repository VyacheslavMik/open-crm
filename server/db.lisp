(in-package #:server.core)

(defparameter *base-db-params*     (list "postgres" "postgres" "localhost" :port 5460))
(defparameter *postgres-db-params* (cons "postgres" *base-db-params*))
(defparameter *system-db-params*   (cons "system" *base-db-params*))

(defun hotel-db-params (hotel-id)
  (cons hotel-id *base-db-params*))

(defparameter *working-db-params* nil)

(defmacro with-default-db (&body body)
  `(let ((*working-db-params* *postgres-db-params*))
     ,@body))

(defmacro with-system-db (&body body)
  `(let ((*working-db-params* *system-db-params*))
     ,@body))

(defmacro with-hotel-db (hotel-id &body body)
  `(let ((*working-db-params* (hotel-db-params ,hotel-id)))
     ,@body))

(defmacro with-connection (&body body)
  `(progn
     (assert *working-db-params* () "DB is not set")
     (postmodern:with-connection *working-db-params*
       ,@body)))

(defmacro query (query &rest args/format)
  `(with-connection (postmodern:query ,query ,@args/format)))

(defmacro make-dao (&rest rest)
  `(with-connection (postmodern:make-dao ,@rest)))

(defmacro execute (query &rest args)
  `(with-connection (postmodern:execute ,query ,@args)))

(defparameter *create-system-db-query*
  "
CREATE DATABASE \"system\"
WITH OWNER \"postgres\"
ENCODING 'UTF8'
LC_COLLATE = 'en_US.UTF-8'
LC_CTYPE = 'en_US.UTF-8'
TEMPLATE template0;")

(defparameter *create-hotel-db-query*
  "
CREATE DATABASE \"~a\"
WITH OWNER \"postgres\"
ENCODING 'UTF8'
LC_COLLATE = 'en_US.UTF-8'
LC_CTYPE = 'en_US.UTF-8'
TEMPLATE template0;")

(defun db-exists-p (name)
  (postmodern:query (:select :datname
		     :from :pg_catalog.pg_database
		     :where (:= :datname name))))

(defun create-system-db ()
  (postmodern:with-connection *postgres-db-params*
    (unless (db-exists-p "system")
      (postmodern:query *create-system-db-query*))))

(defun create-hotel-db (db-id)
  (postmodern:with-connection *postgres-db-params*
    (unless (db-exists-p db-id)
      (postmodern:query (format nil *create-hotel-db-query* db-id)))))

(defparameter *system-tables* ())
(defparameter *hotel-tables* ())

(defun init-tables (tables)
  (dolist (init-table (reverse tables))
    (funcall init-table)))
