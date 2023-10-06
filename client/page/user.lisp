(in-package #:client.core)

(def-form :user-form
  (submit
   (let* ((user (@ this state form-value))
	  (new-user-p (or (null (@ user id))
			  (eql (@ user id) undefined)))
	  (blank-password-p (blank-stringp (@ user password))))
     (cond
       ((and new-user-p blank-password-p)
	(show-error "Пароль обязателен"))

       (new-user-p
	(create-hotel-user user (@ this props on-save)))

       (t
	(update-hotel-user user (@ this props on-save))))))

  (render
   ((:div class (:form :user-form))

    (:div
     ;; TODO: Make the login read only when user is registered
     ;; via sign up mechanism.
     (form-field :text-input
		 (login)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (:label         "Логин"
		  :description   "Введите логин"
		  :class         "login"))
     (form-field :select-input
		 (role)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (:label       "Роль"
		  :description "Выберите роль"
		  :class       "role"
		  :items       (array (create key "admin"
					      label "Администратор")
				      (create key "driver"
					      label "Водитель")
				      (create key "cleaner"
					      label "Уборщица"))))
     (form-field :text-input
		 (password)
		 ()
		 (:label       "Пароль"
		  :description "Введите пароль"
		  :type        "password"
		  :class       "password")))

    ((:div class :buttons)

     ((:contained-button class :submit-button
			 on-click (@ this submit))
      "Сохранить")

     ((:outlined-button class :close-button
			on-click (@ this props on-close))
      "Закрыть")))))

(def-page :admin-user-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 show-form-p nil
				 form-value nil
				 users #())))

  (fetch-users ()
    (chain this (set-state (create status :loading)))
    (fetch-hotel-users
     (http-handler (v)
       (chain this (set-state (create users v
				      status :loaded))))))

  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))

  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-users)))

  (component-did-mount ()
    (chain this (fetch-users)))

  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Пользователи")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t
							       form-value (create hotel-id *hotel-id*))))))
       "+ Добавить"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))

       ((@ this state show-form-p)
	((:user-form on-close (event-handler ()
				(chain this (set-state
					     (create show-form-p nil
						     form-value nil))))
		     form-value (@ this state form-value)
		     on-save (@ this on-save))))

       ((= (@ this state status) :loaded)
	(foreach (user (@ this state users))
	  ((:div class :entity
		 on-click (event-handler ()
			    (chain this (set-state (create show-form-p t
							   form-value user))))
		 key (@ user id))
	   (@ user login)
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-hotel-user user (@ this fetch-users))))
	    "Удалить"))))))))

(def-page :root-user-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 users #())))

  (fetch-users ()
    (chain this (set-state (create status :loading)))
    (fetch-user
     (http-handler (v)
       (chain this (set-state (create users v
				      status :loaded))))))

  (component-did-mount ()
    (chain this (fetch-users)))
			       
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Пользователи"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))

       ((= (@ this state status) :loaded)
	(foreach (user (@ this state users))
	  ((:div class :entity
		 key (@ user id))
	   (@ user login))))))))
