(in-package #:client.core)

(def-page :booking-source-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 sources #()
				 source "")))
  (fetch-booking-sources ()
    (chain this (set-state (create status :loading)))
    (fetch-booking-source
     (http-handler (v)
       (chain this (set-state (create sources v
       				      status :loaded))))))
  (component-did-mount ()
    (chain this (fetch-booking-sources)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Источники бронирований"))
     (if (= (@ this state status) :loading)
	 (:div "Загрузка")
	 ((:div class :column)
	  ((:div class :new)
	   ((:text-input name (@ this state source)
			 error (@ this state error)
			 label "Источник"
			 description "Введите название источника бронирований"
			 validators (array (lambda (v) (unless v "Поле обязательно")))
			 on-change (event-handler (v)
				     (let (err)
				       (unless v
					 (setf err "Поле обязательно"))
				       (chain this (set-state (create error err
								      source v)))))))
	   ((:text-button class :add-booking-source
			  on-click (event-handler ()
				     (unless (@ this state error)
				       (new-booking-source (create name (@ this state source))
							   (@ this fetch-booking-sources)))))
	    "+ Добавить"))
	  (foreach (src (@ this state sources))
	    ((:div class :entity key (@ src id))
	     (@ src name)
	     ((:text-button class :remove
			    on-click (event-handler (e)
				       (chain e (stop-propagation))
				       (delete-booking-source src
							      (http-handler (v)
								(chain this (fetch-booking-sources))))))
	      "Удалить"))))))))
