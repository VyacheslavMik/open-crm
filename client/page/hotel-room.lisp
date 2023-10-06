(in-package #:client.core)

(def-page :hotel-room-page
  (ctor ()
    (setf (@ this state) (create hotel-room-status :loading
				 hotel-rooms #()
				 category-status :loading
				 categories #()
				 category-map (create)
				 hotel-room-name ""
				 hotel-room-category "")))
  (fetch-hotel-rooms ()
    (chain this (set-state (create hotel-room-status :loading)))
    (fetch-hotel-room
     (http-handler (v)
       (chain this (set-state (create hotel-rooms v
       				      hotel-room-status :loaded))))))
  (fetch-categories ()
    (chain this (set-state (create category-status :loading)))
    (fetch-category
     (http-handler (v)
       (chain this (set-state (create categories      (for-select-input v)
				      category-map    (as-hash-map v)
				      category-status :loaded))))))
  (component-did-mount ()
    (chain this (fetch-hotel-rooms))
    (chain this (fetch-categories)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Номера"))
     (if (or (= (@ this state hotel-room-status) :loading)
	     (= (@ this state category-status) :loading))
	 (:div "Загрузка")
	 ((:div class :column)
	  ((:div class :new)
	   ((:text-input value (@ this state hotel-room-name)
			 error (@ this state name-error)
			 label "Название"
			 description "Введите название номера"
			 class :field
			 validators (array (lambda (v) (unless v "Поле обязательно")))
			 on-change (event-handler (v)
				     (let (err)
				       (unless v
					 (setf err "Поле обязательно"))
				       (chain this (set-state (create name-error err
								      hotel-room-name v)))))))
	   ((:select-input value (@ this state hotel-room-category)
			   error (@ this state category-error)
			   label "Категория"
			   description "Выберите категорию номера"
			   validators (array (lambda (v) (unless v "Поле обязательно")))
			   items (@ this state categories)
			   on-change (event-handler (v)
				       (let (err)
					 (unless v
					   (setf err "Поле обязательно"))
					 (chain this (set-state (create category-error err
									hotel-room-category v)))))))
	   ((:text-button on-click (event-handler ()
				     (let (err)
				       (unless (@ this state hotel-room-name)
					 (setf err t)
					 (chain this (set-state
						      (create name-error "Поле обязательно"))))
				       (unless (@ this state hotel-room-category)
					 (setf err t)
					 (chain this (set-state
						      (create category-error "Поле обязательно"))))
				       (unless err
					 (new-hotel-room
					  (create name     (@ this state hotel-room-name)
						  category (@ this state hotel-room-category))
					  (@ this fetch-hotel-rooms))))))
	    "+ Добавить"))
	  (foreach (src (@ this state hotel-rooms))
	    ((:div class :entity key (@ src id))
	     (@ src name)
	     " ("
	     (let ((category (getprop (@ this state category-map) (@ src category))))
	       (if category
		   (@ category name)
		   (@ src category)))
	     ")"
	     ((:text-button class :remove
			    on-click (event-handler (e)
				       (chain e (stop-propagation))
				       (delete-hotel-room src
							  (http-handler (v)
							    (chain this (fetch-hotel-rooms))))))
	      "Удалить"))))))))
