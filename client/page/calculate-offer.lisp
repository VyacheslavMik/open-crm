(in-package #:client.core)

(def-page :calculate-offer-page
  (on-submit (offer)
    (unless (@ offer services)
      (setf (@ offer services) #()))
    (unless (@ offer transfers)
      (setf (@ offer transfers) #()))
    (send-offer offer
		(lambda (v)
		  (show-info "Предложение отправлено"))))
  (render ()
    ((:div class (:column :page :calculate-offer-page))
     ((:div class :header)
      (:h3 "Коммерческое предложение"))
     (:booking-widget
      ((:contained-button class :send-by-email-button
			  on-submit (event-handler (v)
				      (chain this (on-submit v))))
       "Отправить")))))
