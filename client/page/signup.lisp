(in-package #:client.core)

(def-form :signup-form
  (submit
   ;; TODO: Need form state validators not only one field.
   (if (/= (@ this state form-value password)
	   (@ this state form-value confirmation))
       (show-error "Пароль и подтверждение пароля не совпадают")
       (signup (@ this state form-value)
	       (http-handler (v)
		 (chain this props (switch-form))
		 (show-info "Чтобы завершить регистрацию, перейдите по ссылке в письме."))
	       (http-handler (response)
		 (http-process-response
		  response
		  (lambda (o)
		    (if (= o "user-already-exists")
			(show-error "Такой пользователь уже зарегистрирован")
			(show-error o))))))))

  (render
   ((:div class :out-container)

    ((:div class (:out :column))

     (:h2 "Регистрация")

     (form-field :text-input
		 (login)
		 ((lambda (v) (unless v "Это обязательное поле"))
		  (lambda (v)
		    (unless (chain (regexp "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$")
				   (test v))
		      "Неверный формат email")))
		 (label "Логин"
		  description "Введите email"))

     (form-field :text-input
		 (password)
		 ((lambda (v) (unless v "Это обязательное поле"))
		  (lambda (v)
		    (when (< (length v) 8)
		      "Минимальная длина пароля - 8 символов")))
		 (label "Пароль"
		  type "password"
		  description "Введите пароль"))

     (form-field :text-input
		 (confirmation)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Пароль"
		   type "password"
		   description "Повторите пароль"))

     ((:contained-button on-click (@ this submit)
			 class :submit-button)
      "Зарегистрироваться")

     ((:text-button on-click (@ this props switch-form))
      "Вход")))))

;; FIXME: Use new navigation system.
(def-page :signup-page
  (render ()
    ((:div class :signup-page)
     ((:signup-form class :signup-form
		    on-authorized (@ this props on-authorized))))))
