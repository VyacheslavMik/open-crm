(in-package #:client.core)

(def-form :category-form
  (submit
   (unless (@ this state form-value photos)
     (setf (@ this state form-value photos) #()))
   (if (@ this state form-value id)
       (update-category (@ this state form-value)
			(@ this props on-save))
       (new-category (@ this state form-value)
		     (@ this props on-save))))
  (render
   ((:div class (:form :category-form))
    (:div
     (form-field :text-input
		 (name)
		 ((lambda (v) (unless v "Имя категории обязательно")))
		 (label "Название категории"
		  description "Введите название категории"
		  class :category-name))
     (form-field :integer-input
		 (room-count)
		 ((lambda (v) (unless v "Количество номеров обязательно")))
		 (label "Количество номеров"
		  description "Введите количество номеров"
		  class :room-count))
     (form-field :text-input
		 (floors)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Этажи"
		  description "Введите этажи, где находятся эти категории номеров"
		  class :category-floors)))
    (form-field :category-price-input
		(prices)
		((lambda (v) (when (= (length v) 0) "Добавьте хотя бы один период")))
		(label "Стоимость"))
    (form-field :category-photo-input
		(photos)
		()
		(label "Фотографии"))
    ((:div class :buttons)
     ((:contained-button on-click (@ this submit)
			 class :submit-button)
      "Сохранить")
     ((:outlined-button class :close-button
			on-click (@ this props on-close))
      "Закрыть")))))

(def-page :categories-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 show-form-p nil
				 form-value nil
				 categories #())))
  (fetch-categories ()
    (chain this (set-state (create status :loading)))
    (fetch-category
     (http-handler (v)
       (chain this (set-state (create categories v
				      status :loaded))))))
  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))
  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-categories)))
  (component-did-mount ()
    (chain this (fetch-categories)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Категории номеров")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t)))))
       "+ Добавить"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))
       ((@ this state show-form-p)
	((:category-form on-close (event-handler ()
				    (chain this (set-state (create show-form-p nil
								   form-value nil))))
			 form-value (clone-object (@ this state form-value))
			 on-save (@ this on-save))))
       ((= (@ this state status) :loaded)
	(foreach (category (@ this state categories))
	  ((:div class :entity
		 on-click (event-handler ()
			    (chain this (set-state
					 (create show-form-p t
						 form-value category))))
		 key (@ category id))
	   (@ category name)
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-category category
						      (http-handler (v)
							(chain this (fetch-categories))))))
	    "Удалить"))))))))
