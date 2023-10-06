(in-package #:server.core)

(defun telegram/make-url (method)
  (labels ((make-url (telegram-method)
	     (format nil
		     "https://api.telegram.org/bot~a/~a"
		     (config/get :telegram :bot-token)
		     telegram-method)))
    (ecase method
      (:send-message
       (make-url "sendMessage")))))

(defun telegram/get (method &optional params)
  (let* ((url (telegram/make-url method))
	 (response (cl-json:decode-json-from-string
		    (map 'string #'code-char
			 (drakma:http-request url
					      :parameters params
					      :content-type "application/json")))))
    (unless (get-v response :ok)
      (error "Telegram send message error: ~a" (get-v response :description)))
    response))

(defun telegram/send-message (message &rest args)
  (handler-case
      (let ((msg (apply #'format nil message args)))
	(telegram/get :send-message `(("chat_id" . ,(config/get :telegram :receiver))
				      ("text" . ,msg))))
    (error (e)
      (format t "Telegram error: ~a" e)
      (error "Can't send message to telegram."))))
