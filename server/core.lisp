(in-package #:server.core)

(setf json:*json-array-type* 'vector)

(defvar *http-acceptor*)

(defparameter *service-title* "OPEN CRM")

(defun start ()
  (stop)
  (config/load)
  (create-system-db)
  (with-system-db
    (init-tables *system-tables*)
    (init-admin))
  (setf *http-acceptor* (make-instance 'hunchentoot:easy-acceptor :port 4242))
  (hunchentoot:start *http-acceptor*))

(defun stop ()
  (when (and (boundp '*http-acceptor*) *http-acceptor*)
    (hunchentoot:stop *http-acceptor* :soft t)
    (setf *http-acceptor* nil)))

(let ((output *standard-output*))
  (defun log-message (message &rest args)
    (apply #'format output (concatenate 'string "~%~%" message "~%~%") args)))

(defparameter *tokens* ())

(defun make-new-token ()
  (loop for token = (make-uuid)
	while (assoc token *tokens*)
	finally (return token)))

(defroute (:get "/check-authorization") ((:roles :root :admin :driver :cleaner)
					 (:db . any))
  "ok")

(defroute (:post "/signin") ((:public . t)
			     (:db . any))
  (with-system-db
    (let* ((params body)
	   (loging (string-downcase
		    (get-v params :login)))
	   (user   (find-user loging)))
      (if (and user (check-password (get-v params :password)
				    (get-v user :password)))
	  (let ((token (make-new-token)))
	    (push (cons token user) *tokens*)
	    (cons (cons :token token)
		  (remove :password user :key #'car)))
	  (progn
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    "User not found")))))

(defroute (:post "/signup") ((:public . t))
  (let* ((params   body)
	 (email    (string-downcase
		    (get-v params :login)))
	 (user     (find-user email))
	 (password (get-v params :password)))
    (cond
      (user
       (bad-request "user-already-exists"))

      ((invalid-email-p email)
       (bad-request "invalid-email"))

      ((invalid-password-p password)
       (bad-request "invalid-password"))

      (t
       (let* ((params `((:login    . ,email)
			(:role     . "admin")
			(:password . ,(hash-password password))))
	      (user (create-entity 'user params)))
	 (send-email email
		     (format nil "Перейдите по ссылке, чтобы завершить регистрацию: ~a/signup/~a"
			     (server-url)
			     (get-v user :id))
		     "Регистрация"
		     *service-title*)
	 :ok)))))

(defroute (:get "/signup/:id") ((:type . :html)
				(:public . t))
  (let* ((id (get-v params :id))
	 (user (car (read-entity 'user :id id :as 'list)))
	 (message (if (get-v user :confirmedp)
		      "Пользователь уже зарегистрирован"
		      "Регистрация завершена успешно")))

    (unless (get-v user :confirmedp)
      (setf (get-v user :confirmedp) t)
      (update-entity 'user id user))

    (labels ((s (&rest props)
	       (format nil "~{~(~a~): ~a;~^ ~}" props)))
      (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
	(:html
	 :style (s :width "100%"
		   :height "100%")
	 (:head (:title (cl-who:str *service-title*))
		(:link :href "https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900&amp;subset=cyrillic,cyrillic-ext" :rel "stylesheet")
		(:script :src "https://unpkg.com/react@17/umd/react.development.js" :crossorigin t)
		(:script :src "https://unpkg.com/react-dom@17/umd/react-dom.development.js" :crossorigin t)
		(:script :type "text/javascript" :src "/js/client.js"))
	 (:body
	  :style (s :width "100%"
		    :height "100%"
		    :padding 0
		    :margin 0)
	  (:div
	   :style (s :width "100%"
		     :height "100%"
		     :background "rgba(0,0,0,0.4)"
		     :display "flex")
	   (:span
	    :style (s :margin "auto"
		      :padding "40px 80px"
		      :background "white"
		      :border-radius "8px"
		      :font-size "24px"
		      :font-weight "bold")
	    (cl-who:str message)))))))))

(defun authorized-user (authorization)
  (multiple-value-bind (foundp a) (cl-ppcre:scan-to-strings "^Bearer (.+)" authorization)
    (when foundp
      (let ((token (elt a 0)))
	(cdr (assoc token *tokens* :test #'string=))))))

(defparameter *empty-json* (make-hash-table))
(defparameter *image-folder* "../image/")
(defparameter *static-image-folder* "static/image/")

(defun content-type (type)
  (ecase type
    (:html       "text/html")
    (:javascript "application/javascript")
    (:css        "text/css")
    (:json       "application/json")
    (:png        "image/png")))

(defun parse-query-string (qs)
  (let (body)
    (flet ((parse-param (s)
	     (destructuring-bind (k v) (cl-ppcre:split "=" s)
	       (push (cons (make-keyword k) v) body))))
      (dolist (param (cl-ppcre:split "&" qs))
	(parse-param param)))
    body))

(defun body (method query-string request route)
  (ecase method
    (:get
     (parse-query-string query-string))

    ((:post :patch)
     (values
      (if (get-v route :binary)
	  (hunchentoot:raw-post-data :request request :force-binary t)
	  (cl-json:decode-json-from-string
	   (hunchentoot:raw-post-data :request request :force-text t)))
      (parse-query-string query-string)))

    (:delete
     ())))

(defun user-role (user)
  (and user (make-keyword (get-v user :role))))

(defun user-has-access-to-database-p (route user db-params)
  (let ((db (get-v route :db)))
    (cond
      ((eq db 'any)
       t)

      ((and (eq db 'system)
	    (eq db-params *system-db-params*))
       t)

      (t
       (let ((db-id (car db-params)))
	 (find db-id (get-v user :hotels) :test #'string=))))))

;; TODO: Make the function efficient. Use a set of the hotel ids
;; and set of the roles. Maybe it will be more efficient to store
;; user context on the server side.
(defun access-deniedp (route user db-params)
  (cond
    ((null user)
     (not (get-v route :public)))

    ((string= (user-role user) :root)
     nil)

    ((not (get-v user :confirmedp))
     t)

    ((not (user-has-access-to-database-p route user db-params))
     t)

    (t
     (let ((roles (get-v route :roles)))
       (when roles
	 (not (member (user-role user) roles)))))))

(defun hotel-exists-p (db-id)
  (with-system-db
    (with-connection
	(query (:select :id
		:from :hotel
		:where (:= :id db-id))))))

(defun db-params (hotel-id)
  (cond
    ((null hotel-id)
     *system-db-params*)

    ((hotel-exists-p hotel-id)
     (hotel-db-params hotel-id))))

(defmethod hunchentoot:acceptor-dispatch-request :around ((acceptor hunchentoot:easy-acceptor) request)
  (destructuring-bind (uri &optional qs) (cl-ppcre:split "\\?" (hunchentoot:request-uri request))
    (let* ((method (hunchentoot:request-method request))
	   (route  (find-route method uri)))
      (if (null route)
	  (progn
	    (setf (hunchentoot:content-type*) "application/json")
	    (setf (hunchentoot:return-code*) hunchentoot:+http-not-found+)
	    (cl-json:encode-json-to-string "Route not found"))
	  (let* ((params    (get-v route :params))
		 (route     (get-v route :route))
		 (type      (or (get-v route :type) :json))
		 (function  (get-v route :function))
		 (user      (authorized-user
			     (hunchentoot:header-in :authorization request)))
		 (hotel-id  (hunchentoot:header-in :x-hotel-id request))
		 ;; TODO: Review security of the routes.
		 (db-params (db-params hotel-id)))
	    (cond
	      ((null db-params)
	       (setf (hunchentoot:content-type*) "application/json")
	       (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	       (cl-json:encode-json-to-string "Wrong database params"))

	      ((access-deniedp route user db-params)
	       (setf (hunchentoot:content-type*) "application/json")
	       (setf (hunchentoot:return-code*) hunchentoot:+http-forbidden+)
	       (cl-json:encode-json-to-string "Access denied"))
	       

	      ((eql type :static-folder)
	       (call-next-method))

	      ((eql type :continue)
	       (call-next-method))

	      ((eql type :static-file)
	       (funcall function))

	      (t
	       (let ((*working-db-params* db-params))
		 (setf (hunchentoot:content-type*) (content-type type))
		 (multiple-value-bind (body aux) (body method qs request route)
		   (let* ((route (cons (cons :user user) route))
			  (result (funcall function route params body aux)))
		     (if (eql type :json)
			 (if result
			     (cl-json:encode-json-to-string result)
			     (cl-json:encode-json-to-string *empty-json*))
			 result)))))))))))

(defun make-index-page ()
  (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
    (:html
     (:head (:title (cl-who:str *service-title*))
	    (:link :rel "stylesheet" :href "styles.css")
	    (:link :href "https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900&amp;subset=cyrillic,cyrillic-ext" :rel "stylesheet")
	    (:script :src "https://unpkg.com/react@17/umd/react.development.js" :crossorigin t)
	    (:script :src "https://unpkg.com/react-dom@17/umd/react-dom.development.js" :crossorigin t)
	    (:script :type "text/javascript" :src "/js/client.js"))
     (:body :onload (ps:ps (client.core::init)) (:div :id "root")))))

(defroute (:get "/") ((:type . :html)
		      (:public . t))
  (hunchentoot:redirect "/about"))

(defroute (:get "/about") ((:type . :html)
			   (:public . t))
  (make-index-page))

(defroute (:get "/manual") ((:type . :html)
			    (:public . t))
  (make-index-page))

(defroute (:get "/feedback") ((:type . :html)
			      (:public . t))
  (make-index-page))

(defroute (:get "/app") ((:type . :html)
			 (:public . t))
  (make-index-page))

(defroute (:get "/js/client.js") ((:type . :javascript)
				  (:public . t))
  (apply
   #'concatenate
   'string
   (ps:ps* ps:*ps-lisp-library*)
   (mapcar
    (lambda (f)
      (parenscript:ps-compile-file f))
    '("client/utils.lisp"
      "client/date.lisp"
      "client/macro-utils.lisp"
      "client/http.lisp"
      "client/js-class.lisp"
      "client/react.lisp"
      "client/icons.lisp"
      "client/shared.lisp"
      "client/page.lisp"

      "client/component/button.lisp"
      "client/component/menu.lisp"
      "client/component/text-input.lisp"
      "client/component/textarea-input.lisp"
      "client/component/integer-input.lisp"
      "client/component/select-input.lisp"
      "client/component/date-input.lisp"
      "client/component/category-price-input.lisp"
      "client/component/category-photo-input.lisp"
      "client/component/offer-service-input.lisp"
      "client/component/offer-transfer-input.lisp"
      "client/component/header.lisp"

      "client/form.lisp"
      "client/form/booking.lisp"

      "client/page/category.lisp"
      "client/page/service.lisp"
      "client/page/transfer.lisp"
      "client/page/calculate-offer.lisp"
      "client/page/sent-offers.lisp"
      "client/page/new-booking.lisp"
      "client/page/booking.lisp"
      "client/page/bookings.lisp"
      "client/page/template.lisp"
      "client/page/calendar.lisp"
      "client/page/signin.lisp"
      "client/page/signup.lisp"
      "client/page/user.lisp"
      "client/page/task.lisp"
      "client/page/email-settings.lisp"
      "client/page/booking-source.lisp"
      "client/page/hotel-room.lisp"
      "client/page/upcoming-transfers.lisp"
      "client/page/hotel.lisp"
      "client/page/feedback.lisp"
      "client/page/about.lisp"
      "client/page/manual.lisp"

      "client/core.lisp"))))

(defroute (:get "/styles.css") ((:type . :css)
				(:public . t))
  css)

(defroute (:get "/js/react.js") ((:file . "js/react.development.js")
				 (:public . t)))

(defroute (:get "/js/react-dom.js") ((:file . "js/react-dom.development.js")
				     (:public . t)))

(defroute (:get "/image/") ((:folder . *image-folder*)
			    (:public . t)))

(defroute (:get "/static/image/") ((:folder . *static-image-folder*)
				   (:public . t)))

(defroute (:get "/favicon.ico") ((:file . "static/image/favicon.ico")
				 (:public . t)))

(defun str (o)
  (with-output-to-string (s)
    (format s "~a" o)))

(defun find-key-part (binary)
  (let* ((start (search #(13 10) binary))
	 (end   (Search #(13 10) binary :start2 (1+ start))))
    (subseq binary (+ start 2) end)))

(defun find-value-part (binary)
  (let* ((start (search #(13 10 13 10) binary))
	 (end   (Search #(13 10) binary :from-end t)))
    (subseq binary (+ start 4) end)))

(defun parse-multipart-form-data (binary)
  (let* ((key-part (find-key-part binary))
	 (start (+ (search #(110 97 109 101 61 34) key-part) 6))
	 (end (position 34 key-part :start start))
	 (key (subseq key-part start end))
	 (value (find-value-part binary)))
    (list key value)))

(defun save-file (binary extension)
  (let ((id (string-downcase (str (uuid:make-v4-uuid)))))
    (with-open-file (stream (concatenate 'string *image-folder* id extension)
			    :element-type '(unsigned-byte 8)
			    :direction :output
			    :if-does-not-exist :create)
      (write-sequence binary stream))
    (concatenate 'string id extension)))

(defroute (:post "/upload") ((:binary . t))
  (let ((binary body))
    (destructuring-bind (key value) (parse-multipart-form-data binary)
      (let ((extension (concatenate
			'string
			(map
			 'vector
			 #'code-char
			 (subseq key (position 46 key :from-end t))))))
	(save-file value extension)))))

(defun query-settings (key)
  (let* ((row (cadar (query (:select :* :from :settings))))
	 (settings (and row (cl-json:decode-json-from-source row))))
    (cdr (assoc key settings))))

(defun query-email-settings ()
  (query-settings :email-settings))

(defroute (:get "/email-settings") ()
  (or (query-email-settings) *empty-json*))

(defroute (:post "/email-settings") ()
  (let ((settings body))
    (if (get-v settings :name)
	(progn
	  (if (car (query (:select :* :from :settings)))
	      (query (:update :settings
		      :set :entity (:|| :entity (cl-json:encode-json-to-string
						 (pairlis '(:email-settings)
							  (list settings))))))
	      (query (:insert-into :settings
		      :set :id 0 :entity (cl-json:encode-json-to-string
					  (pairlis '(:email-settings)
						   (list settings))))))
	  "ok")
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	  "Wrong parameters"))))

(defun query-template (name)
  (query-settings name))

(defun template-name (body)
  (make-keyword (format nil "~a-template" (get-v body :name))))

(defroute (:get "/template") ()
  (or (query-template (template-name body)) *empty-json*))

(defroute (:post "/template") ()
  (let ((name (template-name aux))
	(template body))
    (if (and (cdr (assoc :subject template))
	     (cdr (assoc :value   template)))
	(progn
	  (if (car (query (:select :* :from :settings)))
	      (query (:update :settings
		      :set :entity (:|| :entity (cl-json:encode-json-to-string
						 (pairlis (list name)
							  (list template))))))
	      (query (:insert-into :settings
		      :set :id 0 :entity (cl-json:encode-json-to-string
					  (pairlis (list name)
						   (list template))))))
	  "ok")
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	  "Wrong parameters"))))

(defun date (s)
  (multiple-value-bind (s parts) (cl-ppcre:scan-to-strings "(\\d{4})-(\\d{2})-(\\d{2})" s)
    (declare (ignore s))
    (destructuring-bind (&optional year month day) (coerce parts 'list)
      (when (and year month day)
	(concatenate 'string day "." month "." year)))))

(defun transform-offer (offer)
  (macrolet ((json-value (obj &rest path)
	       (reduce (lambda (acc v)
			 `(cdr (assoc ,v ,acc)))
		       path :initial-value obj)))
    (macrolet ((property (to &optional from)
		 `(setf (getf plist ,to)
			,(cond
			   ((eq from nil)
			    `(cdr (assoc ,to offer)))
			   ((listp from)
			    `(json-value offer ,@from))
			   ((keywordp from)
			    `(cdr (assoc ,from offer)))
			   (t
			    from)))))
      (let ((photos (map 'list (lambda (photo)
				 (json-value photo :name))
			 (json-value offer :room-category :photos)))
	    (prices (map 'list (lambda (price)
				 (let (plist)
				   (setf (getf plist :start) (json-value price :start))
				   (setf (getf plist :end)   (json-value price :end))
				   (setf (getf plist :value) (json-value price :value))
				   (setf (getf plist :days)  (json-value price :days))
				   plist))
			 (json-value offer :room-price-periods)))
	    (services (map 'list (lambda (service)
				   (let (plist)
				     (setf (getf plist :name)       (json-value service :service :name))
				     (setf (getf plist :price)      (json-value service :service :price))
				     (setf (getf plist :price-type) (json-value service :service :price-type))
				     plist))
			   (json-value offer :services)))
	    (transfers (map 'list (lambda (transfer)
				    (let (plist)
				      (setf (getf plist :name)  (json-value transfer :transfer :name))
				      (setf (getf plist :price) (json-value transfer :transfer :price))
				      plist))
			    (json-value offer :transfers)))
	    (invoices (map 'list (lambda (invoice)
				   (let (plist)
				     (setf (getf plist :date)       (json-value invoice :date))
				     (setf (getf plist :value)      (json-value invoice :value))
				     (setf (getf plist :recorded)   (json-value invoice :recorded))
				     (setf (getf plist :value-type) (json-value invoice :value-type))
				     plist))
			   (json-value offer :invoices)))
	    (receipts (map 'list (lambda (receipt)
				   (let (plist)
				     (setf (getf plist :date)       (json-value receipt :date))
				     (setf (getf plist :value)      (json-value receipt :value))
				     (setf (getf plist :value-type) (json-value receipt :value-type))
				     plist))
			   (json-value offer :receipts)))
	    (documents (let (plist)
			 (setf (getf plist :last-name)      (json-value offer :documents :last-name))
			 (setf (getf plist :first-name)     (json-value offer :documents :first-name))
			 (setf (getf plist :middle-name)    (json-value offer :documents :middle-name))
			 (setf (getf plist :gender)         (json-value offer :documents :gender))
			 (setf (getf plist :birthdate)      (json-value offer :documents :birthdate))
			 (setf (getf plist :document-type)  (json-value offer :documents :document-type))
			 (setf (getf plist :document-value) (json-value offer :documents :document-value))
			 plist))
	    plist)
	(property :discount)
	(property :phone)
	(property :email)
	(property :first-name)
	(property :middle-name)
	(property :last-name)
	(property :check-in)
	(property :check-out)
	(property :total-days)

	(property :room-category-name   (:room-category :name))
	(property :room-category-floors (:room-category :floors))

	(property :room-category-photos photos)
	(property :room-price-periods   prices)
	(property :services             services)
	(property :transfers            transfers)
	(property :invoices             invoices)
	(property :receipts             receipts)
	(property :documents            documents)
	plist))))

(defparameter *email-template*
  "
<p lquery=\"(text first-name)\"/>
<p lquery=\"(text middle-name)\"/>
<p lquery=\"(text last-name)\"/>

<p>Скидка: <span lquery=\"(text discount)\"/></p>
<p>Телефон: <span lquery=\"(text phone)\"/></p>
<p>Email: <span lquery=\"(text email)\"/></p>
<p>Дата въезда: <span lquery=\"(text (date check-in))\"/></p>
<p>Дата выезда: <span lquery=\"(text (date check-out))\"/></p>
<p>Категория: <span lquery=\"(text room-category-name)\"/></p>
<p>Этажи: <span lquery=\"(text room-category-floors)\"/></p>
<p>Дней: <span lquery=\"(text total-days)\"/></p>

<div>
<span>Фотки:</span>
<ul iterate=\"room-category-photos\">
  <li>
   <img lquery=\"(attr (attr-src) (img *))\"/>
  </li>
</ul>
</div>

<div>
<span>Трансферы:</span>
<ul iterate=\"transfers\">
  <li>
    <p>Описание: <span lquery=\"(text name)\"/></p>
    <p>Цена: <span lquery=\"(text price)\"/></p>
  </li>
</ul>
</div>

<div>
<span>Услуги:</span>
<ul iterate=\"services\">
  <li>
    <p>Описание: <span lquery=\"(text name)\"/></p>
    <p>Цена: <span lquery=\"(text price)\"/></p>
    <p>Тип цены: <span lquery=\"(text price-type)\"/></p>
  </li>
</ul>
</div>

<div>
<span>Цены:</span>
<ul iterate=\"room-price-periods\">
  <li>
    <p>Начало: <span lquery=\"(text (date start))\"/></p>
    <p>Окончание: <span lquery=\"(text (date end))\"/></p>
    <p>Количество дней: <span lquery=\"(text days)\"/></p>
    <p>Цена за день: <span lquery=\"(text value)\"/></p>
  </li>
</ul>
</div>
")

(defun attr-src () "src")

(defun img (path)
  (format nil "~a/image/~a" (server-url) path))

(defun percent-p (value-type)
  (equal value-type "percent"))

(defun utc-now ()
  (multiple-value-bind (ss mm hh d m y) (decode-universal-time (get-universal-time) 0)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d.000Z" y m d hh mm ss)))

(defun save-offer (offer)
  (let ((offer (create-entity 'offer (acons :recorded (utc-now) offer))))
    (get-v offer :id)))

(defroute (:post "/send-offer-by-email") ()
  (let ((email-template (query-template :offer-email-template))
	id err)
    (let ((subject (cdr (assoc :subject email-template)))
	  (value   (cdr (assoc :value email-template))))
      (if (and subject value)
	  (let ((offer body))
	    (let ((*package* (find-package '#:server.core))
		  (plist (transform-offer offer)))
	      (handler-case
		  (progn
		    ;; FIXME: Remove this when mailgun integration will be ready.
		    (when (getf plist :email)
		      (send-email (getf plist :email)
				  (apply #'clip:process-to-string value plist)
				  subject))
		    (setf id (save-offer offer)))
		(error (c)
		  (setf err (format nil "~a" c))))))
	  (setf err "Проверьте настройки шаблона письма")))
    (if err
	(progn
	  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	  err)
	id)))

(defun offer-total-price (offer)
  (let ((room-price (or (cdr (assoc :room-total-price offer))
			(cdr (assoc :room-price offer))
			0))
	(total-days (cdr (assoc :total-days offer)))
	(services   (map 'list (lambda (service)
				 (cdr (assoc :service service)))
			 (cdr (assoc :services offer))))
	(transfers  (map 'list (lambda (transfer)
				 (cdr (assoc :transfer transfer)))
			 (cdr (assoc :transfers offer)))))
    (+ room-price
       (reduce (lambda (acc service)
		 (+ acc
		    (if (string= (cdr (assoc :price-type service)) "per_day")
			(* (cdr (assoc :price service)) total-days)
			(cdr (assoc :price service)))))
	       services :initial-value 0)
       (reduce (lambda (acc transfer)
		 (+ acc (cdr (assoc :price transfer))))
	       transfers :initial-value 0))))

(defun normalize-message (phone message)
  (let ((phone (if (char= (elt phone 0) #\+)
		   (subseq phone 1)
		   phone)))
    (destructuring-bind (message &optional images)
	(cl-ppcre:split (format nil "~%===images===~%") message)
      `(("phone" . ,phone)
      	("message" . ,message)
      	("images" . ,(mapcar
      		      (lambda (s)
      			(format nil "~a~a" *image-folder* s))
      		      (remove-if
      		       (lambda (s) (zerop (length s)))
      		       (cl-ppcre:split #\Newline images))))))))

(defun update-booking (booking)
  (query
   (:update :booking
    :set :entity (cl-json:encode-json-to-string
		  (remove :id booking :key #'car))
	    :where (:= :id (cdr (assoc :id booking)))
	    :returning :*))
  (cdr (assoc :id booking)))

(defmacro send-booking-billing (fmt booking &body body)
  `(let (id err)
     (let ((value (format nil ,fmt
			  (hunchentoot:header-in* "origin")
			  (cdr (assoc :id ,booking)))))
       (handler-case
	   (progn
	     ,@body
	     (setf id (update-booking ,booking)))
	 (error (c)
	   (setf err (format nil "~a" c)))))
     (if err
	 (progn
	   (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
	   err)
	 id)))

(defroute (:post "/send-invoice-by-email") ()
  (let ((email (get-v body :email)))
    (if (str:blankp email)
	(bad-request "booking-has-no-email")
	(send-booking-billing "Выписан счет на оплату: ~a/booking/~a/invoices" body
	  (send-email email value "Счет на оплату")))))

(defroute (:post "/send-receipt-by-email") ()
  (let ((email (get-v body :email)))
    (if (str:blankp email)
	(bad-request "booking-has-no-email")
	(send-booking-billing  "Оплата подтверждена: ~a/booking/~a/receipts" body
	  (send-email email value "Подтверждение оплаты")))))

(defun booking-view (params template)
  (let ((body (let* ((id (cdr (assoc :id params)))
		     (row (car
			   (query (:select :*
				   :from :booking
				   :where (:= :id id))))))
		(if row
		    (let ((o (cl-json:decode-json-from-string (cadr row))))
		      (push (cons 'id (car row)) o)
		      (let ((*package* (find-package '#:server.core))
			    (template (query-template template))
			    (plist (transform-offer o)))
			(apply #'clip:process-to-string (or (cdr (assoc :value template)) "") plist)))
		    "Бронь не найдена"))))
    (cl-who:with-html-output-to-string (*standard-output* nil :prologue t)
      (:html
	(:head (:title (cl-who:str *service-title*))
	       (:link :href "https://fonts.googleapis.com/css?family=Roboto:100,300,400,500,700,900&amp;subset=cyrillic,cyrillic-ext" :rel "stylesheet"))
	(:body (cl-who:str body))))))

(defroute (:get "/booking/:id/invoices") ((:type . :html))
  (booking-view params :invoice-site-template))

(defroute (:get "/booking/:id/receipts") ((:type . :html))
  (booking-view params :receipt-site-template))

(defroute (:get "/booking/:id/agreement") ((:type . :html))
  (booking-view params :agreement-site-template))

(defun today-iso ()
  (multiple-value-bind (%0 mm hh d m y %1 %2 %3) (decode-universal-time
						  (get-universal-time))
    (declare (ignore %0 %1 %2 %3))
    (list
     (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)
     (format nil "~2,'0d:~2,'0d" hh mm))))

(defroute (:get "/upcoming-transfers") ((:roles :admin :driver :cleaner))
  (destructuring-bind (date time) (today-iso)
    (declare (ignore time))
    (or
     (mapcar
      (lambda (row)
	(json:decode-json-from-string (car row)))
      (query (:order-by
	      (:select
	       (:jsonb_build_object
		"phone"      (:-> :entity "phone")
		"lastName"   (:-> :entity "lastName")
		"firstName"  (:-> :entity "firstName")
		"middleName" (:-> :entity "middleName")
		"date" (:-> :transfer "transfer" "date")
		"time" (:-> :transfer "transfer" "time")
		"name" (:-> :transfer "transfer" "name")
		"type" (:-> :transfer "transfer" "type")
		"room" :room)
	       :from (:as
		      (:select (:as :booking.entity :entity)
			       (:as (:jsonb-array-elements
				     (:-> :booking.entity "transfers"))
				    :transfer)
			       (:as (:->> :hotel-room.entity "name") :room)
			       :from 'booking
			       :left-join 'hotel-room
			       :on (:= (:->> :booking.entity "room") :hotel-room.id))
		      :q)
	       ;; FIXME may need to add filter by time also
	       :where (:> (:#>> :transfer "{transfer,date}") date))
	      (:#>> :transfer "{transfer,date}") (:#>> :transfer "{transfer,time}"))))
     #())))

(defroute (:get "/my-tasks") ((:roles :admin :driver :cleaner))
  (let ((user (get-v route :user)))
    (or
     (mapcar
      (lambda (row)
	(cons (cons :id (car row))
	      (json:decode-json-from-string (cadr row))))
      (query (:order-by
	      (:select :*
	       :from 'task
	       :where (:= (:->> :entity "user") (cdr (assoc :id user))))
	      (:->> :entity "date") (:->> :entity "time"))))
     #())))

(defroute (:get "/admin-hotels") ((:roles :admin)
				  (:db . system))
  (with-system-db
    (let* ((user-id     (get-v route :user :id))
	   (user        (get-user user-id))
	   (hotel-ids   (get-v user :hotels))
	   (user-hotels (query (:select :*
				:from :hotel
				:where (:= :id (:any* '$1)))
			       (or hotel-ids #()))))
      (map 'vector #'row->json user-hotels))))

;; TODO: Add a input value schema validation.
(defroute (:post "/create-admin-hotel") ((:roles :admin)
					 (:db . system))
  ;; TODO: Add a validation that the user can create hotels.
  ;; TODO: Update the user info stored in the memory (reload user).
  (with-system-db
    (let* ((hotel    (create-entity 'hotel body))
	   (hotel-id (get-v hotel :id)))
      (create-hotel-db hotel-id)
      (with-hotel-db hotel-id
	(init-tables *hotel-tables*))
      (let* ((id     (get-v route :user :id))
	     (user   (get-user id))
	     (hotels (get-v user :hotels)))
	(setf (get-v user :hotels)
	      (coerce (cons hotel-id (coerce hotels 'list))
		      'vector))
	(update-entity 'user id user))
      "ok")))

(defroute (:post "/update-admin-hotel") ((:roles :admin)
					 (:db . system))
  (let ((id (get-v body :id)))
    (update-entity 'hotel id body)))

(defroute (:get "/hotel-users") ((:roles :admin)
				 (:db . system))
  (let ((hotel-id (get-v body :hotel-id)))
    (with-system-db
      (map 'vector
	   (lambda (row)
	     (let ((user (row->json row)))
	       (remove :password user :key #'car)))
	   (query (:select :*
		   :from :user
		   :where (:? (:jsonb (:->> :entity "hotels")) hotel-id)))))))

(defroute (:post "/create-hotel-user") ((:roles :admin)
					(:db . system))
  (with-system-db
    (let* ((login    (get-v body :login))
	   (user     (find-user login))
	   (password (get-v body :password))
	   (hotel-id (get-v body :hotel-id))
	   (role     (get-v body :role)))
      (cond
	(user
	 (bad-request "user-already-exists"))

	((and (str:non-blank-string-p login)
	      (str:non-blank-string-p password)
	      (str:non-blank-string-p role)
	      (str:non-blank-string-p hotel-id))
	 (create-entity 'user `((:login . ,(string-downcase login))
				(:password . ,(hash-password password))
				(:confirmedp . t)
				(:hotels . ,(vector hotel-id))
				(:role . ,role)))
	 "ok")

	(t
	 (bad-request "wrong-parameters"))))))

(defroute (:post "/delete-hotel-user") ((:roles :admin)
					(:db . system))
  ;; FIXME: Add validation that will be deleted user from the
  ;; hotel that is under responsibility of the initiator.
  (with-system-db
    (let ((id (get-v body :id)))
      (delete-entity 'user :id id)
      "ok")))

(defroute (:post "/update-hotel-user") ((:roles :admin)
					(:db . system))
  ;; TODO: Add merge function that merges two jsons.
  (with-system-db
    (let* ((updating-user body)
	   (user-id       (get-v updating-user :id))
	   (old-user      (get-user user-id))
	   (password      (get-v updating-user :password))
	   (role          (get-v updating-user :role))
	   (login         (unless (str:blankp (get-v updating-user :login))
			    (string-downcase
			     (get-v updating-user :login)))))
      (labels ((login-taken-p (login)
		 (unless (str:blankp login)
		   (let ((user (find-user login)))
		     (and user (string/= (get-v user :id) user-id))))))
	(cond
	  ((login-taken-p login)
	   (bad-request "user-already-exists"))

	  ((or (str:non-blank-string-p login)
	       (str:non-blank-string-p password)
	       (str:non-blank-string-p role))

	   (unless (str:blankp login)
	     (setf (get-v old-user :login) login))
	   (unless (str:blankp password)
	     (setf (get-v old-user :password) (hash-password password)))
	   (unless (str:blankp role)
	     (setf (get-v old-user :role) role))

	   (update-entity 'user user-id old-user)

	   "ok")

	  (t
	   (bad-request "wrong-parameters")))))))

(defroute (:post "/offer-to-booking") ((:roles :admin))
  (let ((offer (get-entity 'offer (get-v body :id))))
    (setf (get-v offer :offer) (get-v offer :id))
    (setf offer (rem-v offer :id))
    (get-v (create-entity 'booking offer) :id)))

(defroute (:post "/leave-feedback") ((:public . t)
				     (:db . any))
  (with-system-db
    (create-entity 'feedback body)
    (telegram/send-message "Feedback has been received~%Contact: ~a~%~%Message:~%~a"
			   (get-v body :contact)
			   (get-v body :message))

    "ok"))
