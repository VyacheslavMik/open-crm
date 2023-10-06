(in-package #:client.core)

(def-page :bookings-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 bookings #())))

  (fetch-bookings ()
    (chain this (set-state (create status :loading)))
    (fetch-booking
     (http-handler (v)
       (shared/set bookings v)
       (chain this (set-state (create status :loaded))))))

  (component-did-mount ()
    (shared/subscribe bookings)
    (chain this (fetch-bookings)))

  (component-will-unmount ()
    (shared/unsubscribe))

  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class (:header :booking))
      (:h3 "Брони"))
     (if (= (@ this state status) :loading)
	 (:div "Загрузка")
	 (foreach (booking (@ this state bookings))
	   ((:div class :entity
		  on-click (event-handler ()
			     (goto-page :booking-page (create id (@ booking id)
							      tab :editing)))
		  key (@ booking id))
	    (@ booking last-name) " "
	    (@ booking first-name) " "
	    (@ booking middle-name) " "
	    "("
	    (@ booking check-in)
	    " - "
	    (@ booking check-out)
	    ")"
	    ((:text-button class :remove
			   on-click (event-handler (e)
				      (chain e (stop-propagation))
				      (delete-booking booking
						      (@ this fetch-bookings))))
	     "Удалить")))))))
