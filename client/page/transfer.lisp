(in-package #:client.core)

(def-form :transfer-form
  (submit
   (if (@ this state form-value id)
       (update-transfer (@ this state form-value)
			(@ this props on-save))
       (new-transfer (@ this state form-value)
		     (@ this props on-save))))
  (render
   ((:div class (:form :transfer-form))
    (:div
     (form-field :select-input
		 (type)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Тип трансфера"
		  description "Выберите тип трансфера"
		  class :type
		  items (array (create key "departure"
				       label "Отъезд")
			       (create key "arrival"
				       label "Заезд"))))
     (form-field :text-input
		 (name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Название трансфера"
		  description "Введите название трансфера"
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

(def-page :transfer-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 show-form-p nil
				 form-value nil
				 transfers #())))
  (fetch-transfers ()
    (chain this (set-state (create status :loading)))
    (fetch-transfer
     (http-handler (v)
       (chain this (set-state (create transfers v
				      status :loaded))))))
  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))
  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-transfers)))
  (component-did-mount ()
    (chain this (fetch-transfers)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Трансферы")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t)))))
       "+ Добавить"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))
       ((@ this state show-form-p)
	((:transfer-form on-close (event-handler ()
				    (chain this (set-state (create show-form-p nil
								   form-value nil))))
			 form-value (@ this state form-value)
			 on-save (@ this on-save))))
       ((= (@ this state status) :loaded)
	(foreach (transfer (@ this state transfers))
	  ((:div class :entity
		 on-click (event-handler ()
			    (chain this (set-state (create show-form-p t
							   form-value transfer))))
		 key (@ transfer id))
	   (@ transfer name)
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-service transfer
						     (@ this fetch-transfers))))
	    "Удалить"))))))))
