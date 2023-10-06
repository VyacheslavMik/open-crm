(in-package #:client.core)

(def-widget :offer-service-input
  (ctor ()
    (setf (@ this state) (create error nil
				 service "")))
  (render ()
    ((:div class (:offer-serivce-input :column (@ this props class-name)))
     ((:div class :inputs)
      ((:select-input label "Услуги"
		      description "Добавьте услугу"
		      value (@ this state service)
		      error (@ this state error)
		      items (@ this props items)
		      no-elements-text "Нет услуг"
		      on-change (event-handler (v)
				  (chain this (set-state (create error nil
								 service v))))))
      ((:div class :add-button)
       ((:text-button on-click (event-handler ()
				 (if (@ this state service)
				     (progn
				       (chain this
					      (set-state
					       (create
						service "")))
				       (chain this props
					      (on-change
					       (let ((value (or (@ this props value) #())))
						 (chain value
							(concat
							 (create
							  id (next-id)
							  service (getprop (@ this props service-map)
									   (@ this state service)))))))))
				     (chain this (set-state (create error "Выберите услугу"))))))
	"+ Добавить")))
     (when (@ this props value)
       (let ((days (if (and (@ this props check-out)
			    (@ this props check-in))
		       (date-diff-in-days (parse-date (@ this props check-out))
					  (parse-date (@ this props check-in)))
		       0)))
	 (foreach (item (@ this props value))
	   (let ((service (@ item service)))
	     ((:div key (@ item id) class :offer-service)
	      ((:text-button on-click (event-handler ()
					(chain this props
					       (on-change
						(chain this props value
						       (filter (lambda (v)
								 (/= (@ v id) (@ item id)))))))))
	       "-")
	      (@ service name)
	      (when (= (@ service price-type) "per_day")
		" ")
	      (when (= (@ service price-type) "per_day")
		days)
	      (when (= (@ service price-type) "per_day")
		" x ")
	      (when (= (@ service price-type) "per_day")
		(@ service price))
	      " = "
	      (if (= (@ service price-type) "per_day")
		  (* days (@ service price))
		  (@ service price))))))))))
