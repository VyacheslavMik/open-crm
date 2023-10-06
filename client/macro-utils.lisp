(in-package #:client.utils)

(eval-when (:compile-toplevel)
  (defpsmacro str (&rest args)
    `(chain #',(first args) (concat ,@(rest args))))

  (defpsmacro query (&rest args)
    `(str
      ,@(cdr
	 (apply #'concatenate 'list
		(loop for (k v) on args by #'cddr
		      collect `("&" ,k "=" ,v))))))

  ;; rename to collection
  (defpsmacro foreach (lambda-list &body body)
    `(chain ,(cadr lambda-list)
	    (map
	     (chain (lambda ,(if (listp (car lambda-list))
				 (car lambda-list)
				 `(,(car lambda-list)))
		      ,@body)
		    (bind this)))))

  (defpsmacro event-handler (lambda-list &body body)
    `(chain (lambda ,lambda-list
	      ,@body)
	    (bind this)))

  (defpsmacro pred (lambda-list &body body)
    `(chain (lambda ,lambda-list
	      ,@body)
	    (bind this)))

  (defpsmacro http-handler (lambda-list &body body)
    `(chain (lambda ,lambda-list
	      ,@body)
	    (bind this))))
