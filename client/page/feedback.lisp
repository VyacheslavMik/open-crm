(in-package #:client.core)

(def-form :feedback-form
  (submit
   (http/leave-feedback
    (@ this state form-value)
    (http-handler (_)
      (update-state this
	((form-value contact) "")
	((form-value message) ""))
      (show-info "Сообщение отправлено разработчику"))))

  (render
   ((:div class (:form :feedback-form))

    (form-field :text-input
		(contact)
		((lambda (v) (unless v "Это обязательное поле")))
		(:label "Контакт"
		 :description "Введите контакт"))

    (form-field :textarea-input
		(message)
		((lambda (v) (unless v "Это обязательное поле")))
		(:label "Сообщение"
	      	 :description "Введите сообщение"))

    ((:contained-button class :submit-button
			on-click (@ this submit))
     "Отправить"))))

(def-page :feedback-page
  (render ()
    ((:div class :static-page)

     (:kit/header)

     ((:div class :content)

      (:p "На этой странице вы можете оставить свои вопросы и предложения по системе. А также предложить сотрудничество, если вам интересно развитие данного программного обеспечения.")

      (:p "В поле \"Контакт\" вы можете указать адрес электронной почты, ник в Telegram или номер телефона для связи в WhatsApp.")

      (:feedback-form)))))
