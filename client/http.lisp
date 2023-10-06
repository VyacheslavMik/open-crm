(in-package #:client.utils)

(defun make-url (url query)
  (if query
      (+ url query)
      url))

(defun file-p (o)
  (and o (= (@ o constructor) -file)))

(defun make-form-data (file)
  (let ((res (new (-form-data))))
    (chain res (append (@ file name) file))
    res))

(defvar *token* nil)

(defun http-make-error (response o)
  (chain console (log response))
  (throw (create status      (@ response status)
		 status-text (@ response status-text)
		 body        o)))

(defvar *on-unauthorized* nil)
(defvar *errors* (create "user-already-exists" "Такой пользователь уже зарегистрирован"
			 "booking-has-no-email" "В брони не прописан адрес электронной почты"))

(defun http-process-response (response on-read)
  (let ((ct (chain response headers (get "Content-Type"))))
    (cond 
      ((chain (or ct "") (starts-with "text/html"))
       (chain response (text)
	      (then on-read)))

      ((= ct "application/json")
       (chain response (json)
	      (then on-read)))

      ((= ct "image/png")
       (chain response (blob)
	      (then on-read)))

      (t
       (http-make-error response "Unprocessable content type")))))

(defun http-process-error (response)
  (when (and (= (@ response status) 403) *on-unauthorized*)
    (setf *token* nil)
    (set-cookie :token "")
    (apply *on-unauthorized*))

  (http-process-response
   response
   (lambda (o)
     (http-make-error response o))))

(defun make-headers (props)
  (let ((headers (create "content-type" (if (file-p (@ props body))
					    "multipart/form-data"
					    "application/json")))
	(token (or (@ props token) *token*)))
    (when token
      (setf (getprop headers "Authorization") (+ "Bearer " token)))
    (when *hotel-id*
      (setf (getprop headers "X-Hotel-Id") *hotel-id*))
    headers))

(defun request-server (props)
  (chain
   (fetch
    (make-url (@ props url) (@ props query))
    (create method (@ props method)
	    headers (make-headers props)
	    body (if (file-p (@ props body))
		     (make-form-data (@ props body))
		     (chain -j-s-o-n (stringify (@ props body))))))
   (then
    (lambda (response)
      (cond
	((@ response ok)
	 (http-process-response
	  response
	  (lambda (o)
	    (chain props (on-success o)))))

	((@ props on-error)
	 (let ((r (chain props (on-error response))))
	   (when (eql r :processed)
	     (http-process-error response))))

	(t
	 (http-process-error response)))))
   (catch
       (lambda (err)
	 (chain console (error err))
	 (let* ((body             (@ err body))
		(predefined-error (getprop *errors* body))
		(text-error (cond
			      (predefined-error
			       predefined-error)

			      ((@ err status)
			       (+ (@ err status) " " (@ err status-text) ": " body))

			      ((@ err message)
			       (@ err message))

			      (t
			       "Неизвестная ошибка"))))
	   (if (@ props on-error)
	       (let ((r (chain props (on-error err))))
		 (when (eql r :processed)
		   (chain console (error text-error))
		   (show-error text-error)))
	       (progn
		 (chain console (error text-error))
		 (show-error text-error))))))))

(defun upload-file (file on-success)
  (request-server
   (create url "upload"
	   method "POST"
	   body file
	   on-success on-success)))

(eval-when (:compile-toplevel)
  (defpsmacro def-new-entity (name)
    `(defun ,(make-symbol (concatenate 'string "NEW-" (symbol-name name))) (entity on-success)
       (request-server
	(create url ,(string-downcase (symbol-name name))
		method "POST"
		body entity
		on-success on-success))))

  (defpsmacro def-fetch-entity (name)
    `(defun ,(make-symbol (concatenate 'string "FETCH-" (symbol-name name))) (on-success &optional id)
       (request-server
	(create url (if id
			(str ,(string-downcase (symbol-name name)) "/" id)
			,(string-downcase (symbol-name name)))
		method "GET"
		on-success on-success))))

  (defpsmacro def-delete-entity (name)
    `(defun ,(make-symbol (concatenate 'string "DELETE-" (symbol-name name))) (entity on-success)
       (request-server
	(create url (str ,(string-downcase (symbol-name name)) "/" (@ entity id))
		method "DELETE"
		on-success on-success))))

  (defpsmacro def-update-entity (name)
    `(defun ,(make-symbol (concatenate 'string "UPDATE-" (symbol-name name))) (entity on-success)
       (request-server
	(create url (str ,(string-downcase (symbol-name name)) "/" (@ entity id))
		method "POST"
		body entity
		on-success on-success)))))

(def-fetch-entity category)
(def-new-entity category)
(def-delete-entity category)
(def-update-entity category)

(def-fetch-entity service)
(def-new-entity service)
(def-delete-entity service)
(def-update-entity service)

(def-fetch-entity booking-source)
(def-new-entity booking-source)
(def-delete-entity booking-source)
(def-update-entity booking-source)

(def-fetch-entity transfer)
(def-new-entity transfer)
(def-delete-entity transfer)
(def-update-entity transfer)

(def-fetch-entity booking)
(def-new-entity booking)
(def-delete-entity booking)
(def-update-entity booking)

(def-fetch-entity hotel-room)
(def-new-entity hotel-room)
(def-delete-entity hotel-room)
(def-update-entity hotel-room)

(def-fetch-entity offer)
(def-delete-entity offer)

(def-fetch-entity user)
(def-new-entity user)
(def-delete-entity user)
(def-update-entity user)

(def-fetch-entity task)
(def-new-entity task)
(def-delete-entity task)
(def-update-entity task)

(defun send-offer (body on-success)
  (request-server
   (create url "/send-offer-by-email"
	   method "POST"
	   body body
	   on-success on-success)))

(defun send-invoice (body on-success)
  (request-server
   (create url "/send-invoice-by-email"
	   method "POST"
	   body body
	   on-success on-success)))

(defun send-receipt (body on-success on-error)
  (request-server
   (create url "/send-receipt-by-email"
	   method "POST"
	   body body
	   on-success on-success
	   on-error on-error)))

(defun fetch-email-settings (on-success)
  (request-server
   (create url "/email-settings"
	   method "GET"
	   on-success on-success)))

(defun post-email-settings (body on-success)
  (request-server
   (create url "/email-settings"
	   method "POST"
	   body body
	   on-success on-success)))

(defun fetch-template (entity type on-success)
  (request-server
   (create url (+ "/template?name=" entity "-" type)
	   method "GET"
	   on-success on-success)))

(defun post-template (entity type body on-success)
  (request-server
   (create url (+ "/template?name=" entity "-" type)
	   method "POST"
	   body body
	   on-success on-success)))

(defun get-my-tasks (on-success)
  (request-server
   (create url "/my-tasks"
	   method "GET"
	   on-success on-success)))

(defun signin (body on-success)
  (request-server
   (create url "signin"
	   method "POST"
	   body body
	   on-success on-success)))

(defun signup (body on-success on-error)
  (request-server
   (create url "signup"
	   method "POST"
	   body body
	   on-success on-success
	   on-error on-error)))

(defun get-upcoming-transfers (on-success)
  (request-server
   (create url "/upcoming-transfers"
	   method "GET"
	   on-success on-success)))

(defun fetch-admin-hotels (on-success)
  (request-server
   (create url "/admin-hotels"
	   method "GET"
	   on-success on-success)))

(defun create-admin-hotel (body on-success)
  (request-server
   (create url "/create-admin-hotel"
	   method "POST"
	   body body
	   on-success on-success)))

(defun update-admin-hotel (body on-success)
  (request-server
   (create url "/update-admin-hotel"
	   method "POST"
	   body body
	   on-success on-success)))

(defun fetch-hotel-users (on-success)
  (request-server
   (create url (str "/hotel-users?hotel-id=" *hotel-id*)
	   method "GET"
	   on-success on-success)))

(defun create-hotel-user (body on-success)
  (request-server
   (create url "/create-hotel-user"
	   method "POST"
	   body body
	   on-success on-success)))

(defun update-hotel-user (body on-success)
  (request-server
   (create url "/update-hotel-user"
	   method "POST"
	   body body
	   on-success on-success)))

(defun delete-hotel-user (body on-success)
  (request-server
   (create url "/delete-hotel-user"
	   method "POST"
	   body body
	   on-success on-success)))

(defun http/offer-to-booking (id on-success)
  (request-server
   (create url "/offer-to-booking"
	   method "POST"
	   body (create id id)
	   on-success on-success)))

(defun http/leave-feedback (body on-success)
  (request-server
   (create url "/leave-feedback"
	   method "POST"
	   body body
	   on-success on-success)))

(defun check-authorization (props)
  (request-server
   (create url "/check-authorization"
	   method "GET"
	   token (@ props token)
	   on-success (http-handler ()
			(chain props (on-success))
			(chain props (on-finish)))
	   on-error (http-handler (response)
		      (chain props (on-finish))
		      (when (= (@ response status) 403)
			(chain props (on-error))
			:processed)))))
