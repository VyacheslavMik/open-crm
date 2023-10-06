(in-package #:client.core)

(def-form :service-form
  (submit
   (if (@ this state form-value id)
       (update-service (@ this state form-value)
		       (@ this props on-save))
       (new-service (@ this state form-value)
		    (@ this props on-save))))
  (render
   ((:div class (:form :service-form))
    (:div
     (form-field :select-input
		 (price-type)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Тип цены"
		  description "Выберите тип цены"
		  class :price-type
		  items (array (create key "per_day"
				       label "Цена за сутки")
			       (create key "per_item"
				       label "Цена за единицу"))))
     (form-field :text-input
		 (name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Название услуги"
		  description "Введите название услуги"
		  class :name))
     (form-field :integer-input
		 (price)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Цена"
		  description "Введите цену"
		  class :price)))
    ((:div class :buttons)
     ((:contained-button class :submit-button
			 on-click (@ this submit))
      "Сохранить")
     ((:outlined-button class :close-button
			on-click (@ this props on-close))
      "Закрыть")))))

(def-page :service-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 show-form-p nil
				 form-value nil
				 services #())))
  (fetch-services ()
    (chain this (set-state (create status :loading)))
    (fetch-service
     (http-handler (v)
       (chain this (set-state (create services v
				      status :loaded))))))
  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))
  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-services)))
  (component-did-mount ()
    (chain this (fetch-services)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Услуги")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t)))))
       "+ Добавить"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))
       ((@ this state show-form-p)
	((:service-form on-close (event-handler ()
				   (chain this (set-state (create show-form-p nil
								  form-value nil))))
			form-value (@ this state form-value)
			on-save (@ this on-save))))
       ((= (@ this state status) :loaded)
	(foreach (service (@ this state services))
	  ((:div class :entity
		 on-click (event-handler ()
			    (chain this (set-state (create show-form-p t
							   form-value service))))
		 key (@ service id))
	   (@ service name)
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-service service
						     (@ this fetch-services))))
	    "Удалить"))))))))
