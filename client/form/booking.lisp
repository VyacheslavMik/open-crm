(in-package #:client.core)

(defun find-room-price-period (periods date)
  (labels ((parse-period-date (iso-dm)
	     (let ((d (date:iso-dm->date iso-dm)))
	       (setf (date:year d) (date:year date))
	       d)))
    (find (lambda (period)
	    (let ((start (parse-period-date (@ period start)))
		  (end (parse-period-date (@ period end))))
	      (date:d<= start date end)))
	  periods)))

(defun make-price-period (start end value last-p)
  (create id    start
	  start start
	  end   (if last-p
		    end
		    (next-day end))
	  days  (+ (date-diff-in-days end start)
		   (if last-p 0 1))
	  value value))

(defun calculate-room-price-periods (check-in check-out category)
  (when (and check-in check-out category (> check-out check-in))
    (let* ((check-in  (new (-date check-in)))
	   (check-out (new (-date check-out)))
	   (prices    #())
	   (periods   #()))
      (when (> check-out check-in)
	(loop
	  while (<= check-in check-out)
	  do (progn
	       (let ((price (find-room-price-period (@ category prices) check-in)))
		 (chain prices (push
				(create
				 date (new (-date check-in))
				 value (if price (@ price value) 0)))))
	       (next-day check-in)))
	(chain prices (push (create)))
	(let ((start (chain prices 0 date))
	      (value (chain prices 0 value))
	      (end   (chain prices 0 date)))
	  (loop for price in prices
		when (and (/= value price.value)
			  (/= (date-diff-in-days end start) 0))
		  collect (prog1
			      (make-price-period start end value (empty-object-p price))
			    (setf start price.date
				  value price.value))
		do (setf end price.date)))))))

(defun calculate-room-price (periods)
  (when periods
    (chain periods (reduce (lambda (acc item)
			     (+ acc (* (date-diff-in-days (@ item end)
							  (@ item start))
				       (@ item value))))
			   0))))

(defun calculate-discount (price discount)
  (when (and price discount)
    (- price (/ (* price discount) 100))))

(defun calculate-total (form-value)
  (let ((days (if (and (@ form-value check-out)
		       (@ form-value check-in))
		  (date-diff-in-days (parse-date (@ form-value check-out))
				     (parse-date (@ form-value check-in)))
		  0)))
    (when (> days 0)
      (+ (or (@ form-value room-total-price)
	     (@ form-value room-price))
	 (chain (or (@ form-value services)
		    #())
		(reduce (lambda (acc service)
			  (let ((service (@ service service)))
			    (+ acc
			       (if (= (@ service price-type) "per_day")
				   (* (@ service price) days)
				   (@ service price)))))
			0))
	 (chain (or (@ form-value transfers)
		    #())
		(reduce (lambda (acc transfer)
			  (+ acc (@ transfer transfer price)))
			0))))))

(defun calculate-total-days (check-out check-in)
  (date-diff-in-days (parse-date check-out)
		     (parse-date check-in)))

(def-view :price-periods (props)
  ((:div class (@ props class-name))
   ((:table)
    (:thead
     (:tr
      (:td "С")
      (:td "По")
      (:td "Дней")
      (:td "Цена")
      (:td "Итого")))
    (:tbody
     (foreach (period (@ props periods))
       (let* ((start (parse-date (@ period start)))
	      (end   (parse-date (@ period end)))
	      (days  (date-diff-in-days end start)))
	 ((:tr key (@ period id))
	  (:td (unparse-date start))
	  (:td (unparse-date end))
	  (:td days)
	  (:td (@ period value))
	  (:td (* (@ period value) days)))))))))

(def-form :booking-form
  (submit
   (let ((v (@ this state form-value)))
     (unless (@ v transfers)
       (setf (@ v transfers) #()))
     v))

  (component-did-mount
   (let ((room-category (getprop (@ this props category-map)
				 (@ this props form-value room-category-id))))
     (update-state this
       ((form-value room-category) room-category))))

  (render
   ((:div class (:form :offer-form))

    ((:div class :column)

     (:div
      (form-field :text-input
		  (last-name)
		  ()
		  (label "Фамилия"
		   description "Фамилия"))
      (form-field :text-input
		  (first-name)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Имя"
		   description "Имя"))
      (form-field :text-input
		  (middle-name)
		  ()
		  (label "Отчество"
		   description "Отчество")))

     (:div
      (form-field :date-input
		  (check-in)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Дата заезда"
		   description "Дата заезда"
		   on-change (event-handler (v)
			       (let* ((periods
					(calculate-room-price-periods
					 v
					 (@ this state form-value check-out)
					 (@ this state form-value room-category)))
				      (room-price
					(calculate-room-price periods))
				      (room-total-price
					(calculate-discount
					 room-price
					 (@ this state form-value discount)))
				      (total-days
					(calculate-total-days
					 (@ this state form-value check-out)
					 v))
				      (check-out
					(if (< v (@ this state form-value check-out))
					    (@ this state form-value check-out)
					    "")))
				 (update-state this
				   ((form-value check-in)           v)
				   ((form-value check-out)          check-out)
				   ((form-value total-days)         total-days)
				   ((form-value room-price-periods) periods)
				   ((form-value room-price)         room-price)
				   ((form-value room-total-price)   room-total-price)
				   (:on-new-state (v)
				     (when (@ this props on-change)
				       (chain this props (on-change (@ v form-value))))))))
		   class :check-in
		   type :full-date))
      (form-field :date-input
		  (check-out)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Дата выезда"
		   description "Дата выезда"
		   lower-limit (when (@ this state form-value check-in)
				 (@ this state form-value check-in))
		   on-change (event-handler (v)
			       (let* ((periods
					(calculate-room-price-periods
					 (@ this state form-value check-in)
					 v
					 (@ this state form-value room-category)))
				      (room-price
					(calculate-room-price periods))
				      (room-total-price
					(calculate-discount
					 room-price
					 (@ this state form-value discount)))
				      (total-days
					(calculate-total-days
					 v
					 (@ this state form-value check-in))))
				 (update-state this
				   ((form-value check-out) v)
				   ((form-value total-days) total-days)
				   ((form-value room-price-periods) periods)
				   ((form-value room-price) room-price)
				   ((form-value room-total-price) room-total-price)
				   (:on-new-state (v)
				     (when (@ this props on-change)
				       (chain this props (on-change (@ v form-value))))))))
		   class-name :check-out
		   type :full-date)))

     (when (@ this props booking-p)
       (:div
	(:div
	 (form-field :integer-input
		     (adults)
		     ()
		     (label "Взрослых"
		      description "Количество взрослых"
		      class :adults))
	 (form-field :integer-input
		     (children)
		     ()
		     (label "Детей"
		      description "Количество детей"
		      class :children)))))

     (:div
      (form-field :text-input
		  (phone)
		  ()
		  (label "Телефон"
		   description "Телефон"
		   class :phone))
      (form-field :text-input
		  (email)
		  ()
		  (label "Электронная почта"
		   description "Адрес электронной почты"
		   class :email)))

     (when (@ this props booking-p)
       (form-field :select-input
		   (booking-source)
		   ((event-handler (v)
		      (when (@ this props booking-p)
			(unless v "Поле обязательно"))))
		   (label "Источник"
		    description "Источник бронирования"
		    items (@ this props booking-sources)
		    no-elements-text "Нет источников бронирования"
		    class :booking-source)))

     (:div
      (form-field :select-input
		  (room-category-id)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Категория номера"
		   description "Категория номера"
		   no-elements-text "Нет категорий номеров"
		   items (@ this props categories)
		   on-change (event-handler (v)
			       (let* ((room-category
					(getprop (@ this props category-map) v))
				      (periods
					(calculate-room-price-periods
					 (@ this state form-value check-in)
					 (@ this state form-value check-out)
					 room-category))
				      (room-price
					(calculate-room-price periods))
				      (room-total-price
					(calculate-discount
					 room-price
					 (@ this state form-value discount)))
				      (room
					(when (= v (@ this state form-value room-category-id))
					  (@ this state form-value room))))
				 (update-state this
				   ((form-value room-category-id)   v)
				   ((form-value room-category)      room-category)
				   ((form-value room-price-periods) periods)
				   ((form-value room-price)         room-price)
				   ((form-value room-total-price)   room-total-price)
				   ((form-value room)               room)
				   (:on-new-state (v)
				     (when (@ this props on-change)
				       (chain this props (on-change (@ v form-value))))))))
		   class :room-category))
      (form-field :integer-input
		  (discount)
		  ()
		  (label "Скидка (%)"
		   description "Скидка в процентах"
		   on-change (event-handler (v)
			       (let ((room-total-price
				       (calculate-discount
					(@ this state form-value room-price) v)))
				 (update-state this
				   ((form-value discount) v)
				   ((form-value room-total-price) room-total-price)
				   (:on-new-state (v)
				     (when (@ this props on-change)
				       (chain this props (on-change (@ v form-value))))))))
		   class :discount)))

     (when (@ this state form-value room-price-periods)
       ((:price-periods class :room-price-period
			periods (@ this state form-value room-price-periods))))

     (when (@ this state form-value room-price)
       ((:div class :room-price)
	"Итого: " (@ this state form-value room-price)))

     (when (@ this state form-value room-total-price)
       ((:div class :room-total-price)
	"Всего: " (@ this state form-value room-total-price)))

     (when (@ this props booking-p)
       (form-field :textarea-input
		   (room-wishes)
		   ()
		   (label "Пожелания по номеру"
		    description "Введите пожелания по номеру"
		    class :room-wishes)))

     (when (@ this props booking-p)
       (form-field :select-input
		   (room)
		   ()
		   (label "Номер"
		    description "Номер"
		    no-elements-text (if (@ this state form-value room-category-id)
					 "Нет номеров"
					 "Выберите категорию")
		    items (if (@ this state form-value room-category-id)
			      (filter (pred (v)
					(let* ((id (@ v key))
					       (hotel-room (getprop this 'props 'hotel-room-map id))
					       (category (@ hotel-room category)))
					  (= category
					     (@ this state form-value room-category-id))))
				      (@ this props hotel-rooms))
			      #())
		    class :hotel-room)))

     (form-field :offer-transfer-input
		 (transfers)
		 ((lambda (v)
		    (let ((errors (create)))
		      (loop
			for item across v
			for transfer = (@ item transfer)
			do (let ((error (validate-transfer-time transfer)))
			     (when error
			       (setf (getprop errors (@ transfer id)) error))))
		      (unless (empty-object-p errors)
			errors))))
		 (items        (@ this props transfers)
		  transfer-map (@ this props transfer-map)
		  check-in     (@ this state form-value check-in)
		  check-out    (@ this state form-value check-out)
		  class        :transfers))

     (form-field :offer-service-input
		 (services)
		 ()
		 (items       (@ this props services)
		  check-in    (@ this state form-value check-in)
		  check-out   (@ this state form-value check-out)
		  service-map (@ this props service-map)
		  class       :services))

     (let ((total-price (calculate-total (@ this state form-value))))
       (when total-price
	 ((:div class :total-price)
	  "Общая стоимость всех услуг = " total-price))))

    ((:div class :buttons)
     (let ((count (react-children-count this)))
       (foreach ((btn i) (cond ((= count 0) #())
			       ((= count 1) (array (@ this props children)))
			       (t (@ this props children))))
	 (react-clone btn (create key i
				  on-click (event-handler (e)
					     (let ((v (chain this (submit))))
					       (when v
						 (chain btn props (on-submit v)))))))))))))

(def-widget :booking-widget
  (ctor ()
    (setf (@ this state) (create category-status :loading
				 service-status :loading
				 transfer-status :loading
				 source-status :loading
				 hotel-room-status :loading
				 services #()
				 service-map (create)
				 categories #()
				 category-map (create)
				 hotel-rooms #()
				 booking-sources #())))

  (fetch-categories-success (v)
    (chain this (set-state (create categories      (for-select-input v)
				   category-map    (as-hash-map v)
				   category-status :loaded))))

  (fetch-categories ()
    (chain this (set-state (create category-status :loading)))
    (fetch-category (@ this fetch-categories-success)))

  (fetch-services-success (v)
    (chain this (set-state (create services       (for-select-input v)
				   service-map    (as-hash-map v)
				   service-status :loaded))))

  (fetch-services ()
    (chain this (set-state (create service-status :loading)))
    (fetch-service (@ this fetch-services-success)))

  (fetch-transfers-success (v)
    (chain this (set-state (create transfers       (for-select-input v)
				   transfer-map    (as-hash-map v)
				   transfer-status :loaded))))

  (fetch-transfers ()
    (chain this (set-state (create transfer-status :loading)))
    (fetch-transfer (@ this fetch-transfers-success)))

  (fetch-booking-sources-success (v)
    (chain this (set-state (create booking-sources       (for-select-input v)
				   booking-source-status :loaded))))

  (fetch-booking-sources ()
    (chain this (set-state (create booking-source-status :loading)))
    (fetch-booking-source (@ this fetch-booking-sources-success)))

  (fetch-hotel-room-success (v)
    (chain this (set-state (create hotel-rooms       (for-select-input v)
				   hotel-room-map    (as-hash-map v)
				   hotel-room-status :loaded))))

  (fetch-hotel-rooms ()
    (chain this (set-state (create hotel-room-status :loading)))
    (fetch-hotel-room (@ this fetch-hotel-room-success)))

  (component-did-mount ()
    (chain this (fetch-categories))
    (chain this (fetch-services))
    (chain this (fetch-transfers))
    (chain this (fetch-booking-sources))
    (chain this (fetch-hotel-rooms))
    (with-slots (check-in room room-category-id form-value) (@ this props)
      (when (null (@ this state form-value))
	(cond
	  ((or check-in room room-category-id)
	   (update-state this
	     ((form-value check-in) check-in)
	     ((form-value room-category-id) room-category-id)
	     ((form-value room) room)))

	  ((null form-value)
	   (update-state this
	     ((form-value) (create))))

	  (t
	   (update-state this
	     ((form-value) form-value)))))))

  (render ()
    (if (or (= (@ this state category-status)       :loading)
	    (= (@ this state service-status)        :loading)
	    (= (@ this state transfer-status)       :loading)
	    (= (@ this state booking-source-status) :loading)
	    (= (@ this state room-status)           :loading))
	(:div "Загрузка")
	((:booking-form categories      (@ this state categories)
			category-map    (@ this state category-map)
			services        (@ this state services)
			service-map     (@ this state service-map)
			transfers       (@ this state transfers)
			transfer-map    (@ this state transfer-map)
			hotel-rooms     (@ this state hotel-rooms)
			hotel-room-map  (@ this state hotel-room-map)
			booking-sources (@ this state booking-sources)
			booking-p       (@ this props booking-p)
			on-change       (@ this props on-change)
			form-value      (@ this state form-value))
	 (@ this props children)))))
