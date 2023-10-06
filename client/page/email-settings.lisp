(in-package #:client.core)

(def-form :email-settings-form
  (submit
   (post-email-settings (@ this state form-value)
			(lambda (v)
			  (show-info "Настройки сохранены"))))
  (render
   ((:div class (:form :email-settings-form))
    (:div
     (form-field :text-input
		 (name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Имя"
		  description "Введите имя")))
    ((:contained-button class :submit-button
			on-click (@ this submit))
     "Сохранить"))))

(def-page :email-settings-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 form-value nil)))
  (fetch-settings ()
    (chain this (set-state (create status :loading)))
    (fetch-email-settings
     (http-handler (v)
       (chain this (set-state (create form-value (if (empty-object-p v)
						     (create name "")
						     v)
				      status :loaded))))))
  (component-did-mount ()
    (chain this (fetch-settings)))
  (render ()
    ((:div class (:column :page :email-settings-page))
     ((:div class :header)
      (:h3 "Настройки почты"))
     (if (= (@ this state status) :loading)
	 (:div "Загрузка")
	 ((:email-settings-form form-value (@ this state form-value)))))))
