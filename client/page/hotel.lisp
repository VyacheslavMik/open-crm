(in-package #:client.core)

(def-form :hotel-form
  (submit
   (if (@ this state form-value id)
       (update-admin-hotel (@ this state form-value)
			   (@ this props on-save))
       (create-admin-hotel (@ this state form-value)
			   (@ this props on-save))))
  (render
   ((:div class (:form :hotel-form))
    ((:div class :column)
     (form-field :text-input
		 (name)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Название"
		  description "Введите название отеля"))
     (form-field :textarea-input
		 (description)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Описание"
		  description "Введите описание отеля")))
    ((:div class :buttons)
     ((:contained-button class :submit-button
			 on-click (@ this submit))
      "Сохранить")
     ((:outlined-button class :close-button
			on-click (@ this props on-close))
      "Закрыть")))))

(def-page :hotel-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 show-form-p nil
				 form-value nil
				 hotels #())))

  (fetch-hotels ()
    (chain this (set-state (create status :loading)))
    (fetch-admin-hotels
     (http-handler (v)
       (chain this (set-state (create hotels v
				      status :loaded))))))

  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))

  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-hotels)))

  (component-did-mount ()
    (chain this (fetch-hotels)))

  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Отели")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t)))))
       "+ Добавить"))
     (cond
       ((= (@ this state status) :loading)
	(:div "Загрузка"))

       ((@ this state show-form-p)
	((:hotel-form on-close (event-handler ()
				 (chain this (set-state (create show-form-p nil
								form-value nil))))
		      form-value (@ this state form-value)
		      on-save (@ this on-save))))

       ((= (@ this state status) :loaded)
	(foreach (hotel (@ this state hotels))
	  ((:div class :entity
		 on-click (event-handler ()
			    (chain this (set-state (create show-form-p t
							   form-value hotel))))
		 key (@ hotel id))
	   (@ hotel name)
	   (when (/= (@ hotel id) *hotel-id*)
	     ((:text-button class :select
			    on-click (event-handler (e)
				       (chain e (stop-propagation))
				       (setf *hotel-id* (@ hotel id))
				       (set-cookie :hotel-id *hotel-id*)
				       (funcall *on-hotel-selected*)))
	      "Выбрать")))))))))
