(in-package #:client.core)

(def-view :sent-offer-field (props)
  (let ((label (aref (@ props children) 0))
	(value (aref (@ props children) 1)))
    ((:div class :sent-offer-field)
     ((:label class (:label :activated))
      label)
     ((:div class :input)
      (if (blank-stringp value)
	  "-"
	  value)))))

(def-view :sent-offer-transfer-list (props)
  ((:div class :sent-offer-transfer-list)
   ((:label class (:label :activated))
    "Трансферы")
   (:table
    (:thead
     (:tr
      (:td "Направление")
      (:td "Название")
      (:td "Число")
      (:td "Время")
      (:td "Цена")
      (:td "Комментарий")))
    (:tbody
     (foreach (transfer (@ props children))
       (let ((id       (@ transfer id))
	     (transfer (@ transfer transfer)))
	 ((:tr key id)
	  (:td (transfer-type transfer))
	  (:td (@ transfer name))
	  (:td (unparse-date (parse-date (@ transfer date))))
	  (:td (@ transfer time))
	  (:td (@ transfer price))
	  (:td (@ transfer comment)))))))))

(def-view :sent-offer-service-list (props)
  ((:div class :sent-offer-service-list)
   ((:label class (:label :activated))
    "Услуги")
   (:table
    (:thead
     (:tr
      (:td "Название")
      (:td "Дней")
      (:td "Цена")
      (:td "Итого")))
    (:tbody
     (foreach (service (@ props children))
       (let* ((id        (@ service id))
	      (service   (@ service service))
	      (per-day-p (= (@ service price-type) "per_day")))
	 ((:tr key id)
	  (:td (@ service name))
	  (:td
	   (when per-day-p
	     (@ props days)))
	  (:td
	   (when per-day-p
	     (@ service price)))
	  (:td
	   (if per-day-p
	       (* (@ props days) (@ service price))
	       (@ service price))))))))))

(def-widget :offer-view
  (offer-to-booking (_)
    (http/offer-to-booking
     (@ this props value id)
     (http-handler (v)
       (goto-page :booking-page (create id v
					tab :editing)))))

  (render ()
    (let ((value (@ this props value)))
      ((:div class (:column :page :sent-offer-page))
       ((:div class :header)
	(:h3
	 (@ value last-name) " "
	 (@ value first-name) " "
	 (@ value middle-name)))

       (:div
	(:sent-offer-field "Телефон" (@ value phone))
	(:sent-offer-field "Email" (@ value email)))

       (:div
	(:sent-offer-field "Заезд" (unparse-date (parse-date (@ value check-in))))
	(:sent-offer-field "Выезд" (unparse-date (parse-date (@ value check-out)))))

       (:div
	(:sent-offer-field "Категория" (@ value room-category name)))

       (let ((price-periods (calculate-room-price-periods
			     (@ value check-in)
			     (@ value check-out)
			     (@ value room-category))))
	 (:div
	  (when price-periods
	    ((:price-periods class :room-price-period
			     periods price-periods)))))

       (when (@ value room-price)
	 (:div
	  (:sent-offer-field "Итого" (@ value room-price))))

       (when (@ value discount)
	 (:div
	  (:sent-offer-field "Скидка" (+ (@ value discount) "%"))))

       (when (@ value room-total-price)
	 (:div
	  (:sent-offer-field "Всего" (@ value room-total-price))))

       (when (> (length (@ value transfers)) 0)
	 (:sent-offer-transfer-list (@ value transfers)))

       (when (> (length (@ value services)) 0)
	 (let ((days (if (and (@ value check-out)
			      (@ value check-in))
			 (date-diff-in-days (parse-date (@ value check-out))
					    (parse-date (@ value check-in)))
			 0)))
	   ((:sent-offer-service-list days days)
	    (@ value services))))

       (let ((total-price (calculate-total value)))
	 (when total-price
	   (:div
	    (:sent-offer-field "Общая стоимость всех услуг" total-price))))

       ((:div class :controls)

	((:contained-button on-click (@ this offer-to-booking))
	 "Создать бронь")

	((:outlined-button class :close-button
			   on-click (@ this props on-close))
	 "Закрыть"))))))

(def-page :sent-offers-page
  (on-success (v)
    (chain this (set-state (create offers v
				   offer-status :loaded))))

  (fetch-offers ()
    (chain this (set-state (create offer-status :loading)))
    (fetch-offer (@ this on-success)))

  (ctor ()
    (setf (@ this state) (create offer-status :loading
				 offers #()
				 view-value nil)))

  (component-did-mount ()
    (chain this (fetch-offers)))

  (render ()
    (cond
      ((= (@ this state offer-status) :loading)
       (:div "Загрузка"))

      ((@ this state view-value)
       ((:offer-view on-close (event-handler ()
				(chain this (set-state (create view-value nil))))
		     value (@ this state view-value))))

      (t
       ((:div class (:column :page :entities-page))

	((:div class :header)
	 (:h3 "Отправленные предложения"))

	(foreach (offer (@ this state offers))
	  ((:div key (@ offer id)
		 class :entity
		 on-click (event-handler ()
			    (chain this (set-state
					 (create view-value offer)))))
	   ((:div class :column)
	    (:span (date:unparse 'datetime (@ offer recorded)))
	    (:span
	     (@ offer last-name) " "
	     (@ offer first-name) " "
	     (@ offer middle-name)))
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-offer offer
						   (@ this fetch-offers))))
	    "Удалить"))))))))
