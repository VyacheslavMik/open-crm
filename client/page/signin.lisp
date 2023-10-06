(in-package #:client.core)

(def-form :signin-form
  (submit
   (signin (@ this state form-value)
	   (http-handler (v)
	     ;; TODO: The logic behind user signin is splitted by
	     ;; several files and it must be stored in one place.
	     (setf *token* (@ v token))
	     (when (= (length (or (@ v hotels) #())) 1)
	       (setf *hotel-id* (aref (@ v hotels) 0))
	       (set-cookie :hotel-id *hotel-id*))
	     (setf *user*  v)
	     (set-cookie :user v)
	     (setf *authorized-p* t)
	     (chain this props (on-authorized)))))
  (render
   ((:div class :out-container)

    ((:div class (:out :column))

     (:h2 "Вход")

     (form-field :text-input
		 (login)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Логин"
		  description "Введите логин"))

     (form-field :text-input
		 (password)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Пароль"
	      	  type "password"
	      	  description "Введите пароль"))

     ((:contained-button on-click (@ this submit)
			 class :submit-button)
      "Войти")

     ((:text-button on-click (@ this props switch-form))
      "Регистрация")))))

(def-page :signin-page
  (ctor ()
    (setf (@ this state) (create form :signin)))
  (render ()
    ((:div class :out-page)
     (case (@ this state form)
       (:signin
	((:signin-form class :out-form
		       switch-form (event-handler (e)
				     (chain this (set-state (create form :signup))))
		       on-authorized (@ this props on-authorized))))

       (:signup
	((:signup-form class :out-form
		       switch-form (event-handler (e)
				     (chain this (set-state (create form :signin))))
		       on-authorized (@ this props on-authorized))))))))
