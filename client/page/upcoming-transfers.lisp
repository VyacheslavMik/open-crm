(in-package #:client.core)

(def-page :upcoming-transfers-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 transfers #())))

  (fetch-transfers ()
    (chain this (set-state (create status :loading)))
    (get-upcoming-transfers
     (http-handler (v)
       (chain this (set-state (create transfers v
				      status :loaded))))))

  (component-did-mount ()
    (chain this (fetch-transfers)))

  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Трансферы"))
     (if (= (@ this state status) :loading)
	 (:div "Загрузка")
	 (foreach ((transfer id) (@ this state transfers))
	   ((:div class :entity key id)
	    ((:div class :upcoming-transfer)
	     (:span
	      (@ transfer last-name) " "
	      (@ transfer first-name) " "
	      (@ transfer middle-name)
	      " (" (@ transfer phone) ")")
	     (:span
	      (date:unparse 'date (@ transfer date))
	      " в "
	      (@ transfer time))
	     (:span (@ transfer name))
	     (when (cleanerp)
	       (:span "Комната " (@ transfer room))))))))))
