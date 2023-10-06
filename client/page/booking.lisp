(in-package #:client.core)

(def-form :booking-invoice
  (submit
   (@ this state form-value))
  (render
   ((:div class :column)
    ((:div class :header)
     (:h3 "Счет на оплату"))
    ((:div class :inputs)
     (form-field :select-input
		 (value-type)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Процент или сумма"
		  description "Выберите процент или сумму"
		  class :field-input
		  items (array (create key "percent"
				       label "Процент")
			       (create key "scalar"
				       label "Сумма"))))
     (form-field :integer-input
		 (value)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Сумма или процент"
		  description "Введите cумму или процент"
		  class :field-input))
     (form-field :date-input
		 (date)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Оплатить до"
		  description "Выберите до какого числа нужно оплатить"
		  class :field-input
		  type :full-date)))
    (:div
     ((:contained-button class :send-by-email-button
			 on-click (event-handler ()
				    ;; FIXME: Rewrite the whole billing.
				    (if (blank-stringp (@ this props booking email))
					(show-error "В брони не прописан адрес электронной почты")
					(let ((invoice (chain this (submit))))
					  (when invoice
					    (unless (@ this props booking invoices)
					      (setf (@ this props booking invoices) #()))
					    (setf (@ invoice recorded) (parse-date))
					    (chain this props booking invoices (push invoice))
					    (send-invoice (@ this props booking)
							  (event-handler (v)
							    (show-info "Счет отправлен")
							    (chain this props (rerender)))))))))
      "Отправить")))))

(def-widget :booking-invoices
  (render ()
    ((:div class :column)
     ((:div class :header)
      (:h3 "Отправленные счета"))
     (when (@ this props booking invoices)
       (foreach ((invoice id) (@ this props booking invoices))
	 ((:div key id)
	  (date:unparse 'datetime (@ invoice recorded)) ": оплатить "
	  (if (= (@ invoice value-type) "percent")
	      (+ (@ invoice value) "%")
	      (+ (@ invoice value) " рублей"))
	  " до " (date:unparse 'date (@ invoice date))))))))

(def-form :booking-receipt
  (submit
   (@ this state form-value))
  (render
   ((:div class :column)
    ((:div class :header)
     (:h3 "Подтверждение оплаты"))
    ((:div class :inputs)
     (form-field :select-input
		 (value-type)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Источник"
		  description "Выберите откуда получены деньги"
		  class :field-input
		  items (array (create key "sberbank"
				       label "Сбербанк")
			       (create key "checking_account"
				       label "Расчетный счет")
			       (create key "cash"
				       label "Наличные"))))
     (form-field :integer-input
		 (value)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Сумма оплаты"
		  description "Введите cумму оплаты"
		  class :field-input))
     (form-field :date-input
		 (date)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Дата оплаты"
		  description "Выберите когда оплатили"
		  class :field-input
		  type :full-date)))
    (:div
     ((:contained-button class :send-by-email-button
			 on-click (event-handler ()
				    (if (blank-stringp (@ this props booking email))
					(show-error "В брони не прописан адрес электронной почты")
					(let ((receipt (chain this (submit))))
					  (when receipt
					    (unless (@ this props booking receipts)
					      (setf (@ this props booking receipts) #()))
					    (chain this props booking receipts (push receipt))
					    (send-receipt (@ this props booking)
							  (event-handler (v)
							    (show-info "Подтверждение оплаты отправлено")
							    (chain this props (rerender)))
							  (event-handler (_)
							    (chain this props (rerender))
							    ;; FIXME: Why processed does mean that we have to
							    ;; show an error on the http layer?
							    :processed)))))))
      "Отправить")))))

(def-widget :booking-receipts
  (render ()
    ((:div class :column)
     ((:div class :header)
      (:h3 "Подтвержденные оплаты"))
     (when (@ this props booking receipts)
       (foreach ((receipt id) (@ this props booking receipts))
	 ((:div key id)
	  (date:unparse 'date (@ receipt date)) ": оплатили "
	  (+ (@ receipt value) " рублей"))))
     (when (@ this props booking receipts)
       ((:div class :margin))))))

(def-widget :booking-payment
  (ctor ()
    (setf (@ this state) (create id 0)))
  (rerender ()
    (chain this (set-state (lambda (state)
			     (+ (@ state id) 1)))))
  (render ()
    ((:div class (:column :booking-payment))
     ((:booking-invoice booking (@ this props booking)
			rerender (@ this rerender)))
     ((:booking-invoices booking (@ this props booking)))
     ((:booking-receipt booking (@ this props booking)
			rerender (@ this rerender)))
     ((:booking-receipts booking (@ this props booking))))))

(def-form :booking-documents
  (submit
   (@ this state form-value))
  (render
   ((:div class :column)
    ((:div class :header)
     (:h3 "Документы"))
    ((:div class :inputs)
     (form-field :text-input
		 (last-name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Фамилия"
		  description "Введите фамилию"
		  class :field-input))
     (form-field :text-input
		 (first-name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Имя"
		  description "Введите имя"
		  class :field-input))
     (form-field :text-input
		 (middle-name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Отчество"
		  description "Введите отчетство"
		  class-name :field-input)))
    ((:div class :inputs)
     (form-field :select-input
		 (gender)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Пол"
		  description "Выберите пол"
		  class :field-input
		  items (array (create key "male"
				       label "Мужской")
			       (create key "female"
				       label "Женский"))))
     (form-field :date-input
		 (birthdate)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Дата рождения"
		  description "Выберите дату рождения"
		  class :field-input
		  type :full-date)))
    ((:div class :inputs)
     (form-field :select-input
		 (document-type)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Документ"
		  description "Выберите документы"
		  class :field-input
		  items (array (create key "passport"
				       label "Паспорт")
			       (create key "driver_license"
				       label "Водительские права")
			       (create key "other"
				       label "Другое"))))
     (form-field :text-input
		 (document-value)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Данные"
		  description "Введите данные"
		  class :field-input)))
    (:div
     ((:contained-button class :print-agreement
			 on-click (event-handler ()
				    (let ((doc (chain this (submit))))
				      (when doc
					(setf (@ this props booking documents) doc)
					(update-booking
					 (@ this props booking)
					 (event-handler (v)
					   (chain window (open (+ "/booking/"
								  (@ this props booking id)
								  "/agreement")
							       "_blank"))))))))
      "Распечатать договор")))))

(def-form :booking-car
  (submit
   (@ this state form-value))
  (render
   ((:div class :column)
    ((:div class :header)
     (:h3 "Автомобиль"))
    ((:div class :inputs)
     (form-field :text-input
		 (parking)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Стоянка"
		  description "Введите стоянку"
		  class :field-input))
     (form-field :text-input
		 (car-model)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Модель"
		  description "Введите модель"
		  class :field-input))
     (form-field :text-input
		 (number)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Номер"
		  description "Введите номер"
		  class :field-input)))
    (:div
     ((:contained-button on-click (event-handler ()
				    (let ((car (chain this (submit))))
				      (when car
					(setf (@ this props booking car) car)
					(update-booking
					 (@ this props booking)
					 (event-handler (v)
					   (show-info "Бронь сохранена")))))))
      "Сохранить")))))

(def-widget :booking-checkining
  (render ()
    ((:div class (:column :booking-checkining))
     ((:div class :header)
      (:h3 "К оплате: ")
      ((:div class :remains)
       (- (calculate-total (@ this props booking))
	  (chain (or (@ this props booking receipts)
		     #())
		 (reduce (lambda (acc v) (+ acc (@ v value))) 0)))))
     ((:booking-documents booking    (@ this props booking)
			  form-value (@ this props booking documents)))
     ((:booking-car booking (@ this props booking)
		    class :car-form
		    form-value (@ this props booking car))))))

(def-page :booking-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 form-value nil)))

  (return-to-bookings ()
    (goto-page :bookings-page))

  (on-save ()
    (update-booking (@ this state form-value)
		    (@ this return-to-bookings)))

  (on-close (_e)
    (chain this (return-to-bookings)))

  (component-did-mount ()
    (with-slots (form-value) (@ this state)
      (when (or (not form-value)
		(/= (@ form-value id) (@ this props id)))
	(let* ((bookings (shared/get bookings))
	       (booking (find (pred (v)
				(= (@ v id) (@ this props id)))
			      bookings)))
	  (if booking
	      (update-state this
		((form-value) booking)
		((status)     :loaded))
	      (fetch-booking (http-handler (v)
			       (update-state this
				 ((form-value) (aref v 0))
				 ((status)     :loaded)))
			     (@ this props id)))))))

  (render ()
    ((:div class (:column :page :booking))
     ((:div class (:header :booking))
      (:h3 "Бронь")

      ((:div class (when (= (@ this props tab) :paying)
		     :active))
       ((:text-button on-click (event-handler ()
				 (goto-page :booking-page (create id (@ this props id)
								  tab :paying))))
      	"Оплата"))

      ((:div class (when (= (@ this props tab) :checkining)
		     :active))
       ((:text-button on-click (event-handler ()
				 (goto-page :booking-page (create id (@ this props id)
								  tab :checkining))))
      	"Заезд"))

      ((:div class (when (= (@ this props tab) :editing)
		     :active))
       ((:text-button on-click (event-handler ()
				 (goto-page :booking-page (create id (@ this props id)
								  tab :editing))))
	"Редактирование")))

     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))

       ((= (@ this props tab) :editing)
	((:booking-widget form-value (@ this state form-value)
			  on-change (event-handler (v)
				      (update-state this
					((form-value) v)))
			  booking-p t)))
							      
       ((= (@ this props tab) :paying)
	((:booking-payment booking (@ this state form-value))))

       ((= (@ this props tab) :checkining)
	((:booking-checkining booking (@ this state form-value)))))

     (:div

      ((:outlined-button on-click (@ this on-close)
			 class :close)
       "Закрыть")

      (when (= (@ this props tab) :editing)
	((:contained-button on-click (@ this on-save)
			    class :save)
	 "Сохранить"))))))
