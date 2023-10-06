(in-package #:client.core)

(def-page :new-booking-page
  (on-submit (booking)
    (unless (@ booking services)
      (setf (@ booking services) #()))
    (unless (@ booking transfers)
      (setf (@ booking transfers) #()))
    (new-booking booking
		 (lambda (v)
		   (goto-page :booking-page (create id (@ v id)
						    tab :editing)))))

  (render ()
    ((:div class (:column :page :booking-page))

     ((:div class :header)
      (:h3 "Новое бронирование"))

     ((:booking-widget booking-p        t
		       check-in         (@ this props check-in)
		       room-category-id (@ this props room-category-id)
		       room             (@ this props room))
      ((:contained-button class :new-booking
			  on-submit (event-handler (v)
				      (chain this (on-submit v))))
       "Создать бронь")))))
