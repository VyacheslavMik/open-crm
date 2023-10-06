(in-package #:client.core)

(defun transfer-type (transfer)
  (if (= (@ transfer type) "departure")
      "Отъезд из отеля "
      "Заезд в отель "))

(defun validate-transfer-time (transfer)
  (unless (@ transfer time)
    (return "Время трансфера обязательно"))
  (let* ((splitted (chain transfer time (split ":")))
	 (hours (aref splitted 0))
	 (mins (aref splitted 1)))
    (when (or (/= (length (or hours #())) 2)
	      (/= (length (or mins #())) 2))
      (return "Время должно быть в формате hh:mm"))
    (let ((hours (parse-int hours))
	  (mins (parse-int mins)))
      (when (or (not (<= 0 hours 23))
		(not (<= 0 mins 59)))
	(return "Допустимый диапазон от 00:00 до 23:59")))))

(def-widget :offer-transfer-input
  (ctor ()
    (setf (@ this state) (create error nil
				 transfer "")))
  (add ()
    (if (@ this state transfer)
	(progn
	  (chain this
		 (set-state (create transfer "")))
	  (chain this props
		 (on-change
		  (let ((value (or (@ this props value) #()))
			(transfer (clone-object
				   (getprop (@ this props transfer-map)
					    (@ this state transfer)))))
		    (cond
		      ((and (= (@ transfer type) "arrival") (@ this props check-in))
		       (setf (@ transfer date) (@ this props check-in)))

		      ((and (= (@ transfer type) "departure") (@ this props check-out))
		       (setf (@ transfer date) (@ this props check-out)))

		      (t
		       (setf (@ transfer date) (date:now))))
		    (chain value (concat (create
					  id (next-id)
					  transfer transfer)))))))
	(chain this (set-state (create error "Выберите трансфер")))))
  (render ()
    ((:div class (:offer-transfer-input :column (@ this props class-name)))
     ((:div class :inputs)
      ((:select-input label "Трансфер"
		      description "Добавьте трансфер"
		      value (@ this state transfer)
		      error (@ this state error)
		      items (@ this props items)
		      no-elements-text "Нет трансферов"
		      on-change (event-handler (v)
				  (chain this (set-state (create error nil
								 transfer v))))))
      ((:div class :add-button)
       ((:text-button on-click (@ this add))
	"+ Добавить")))
     (when (@ this props value)
       (foreach (item (@ this props value))
	 (let* ((transfer (@ item transfer))
		(errors (or (@ this props error) (create)))
		(error (@ errors (@ transfer id))))
	   ((:div key (@ item id) class (:offer-transfer :column))
	    (:div
	     ((:text-button on-click (event-handler ()
				       (chain this props
					      (on-change
					       (chain this props value
						      (filter (lambda (v)
								(/= (@ v id) (@ item id)))))))))
	      "-")
	     (transfer-type transfer)
	     (@ transfer name) " = "
	     (@ transfer price))
	    ((:div class :date-time)
	     ((:date-input label "Дата"
			   value (or (@ transfer date) "")
			   on-change (event-handler (v)
				       (setf (@ transfer date) v)
				       (chain this props
					      (on-change (@ this props value))))
			   class :date
			   type :full-date))
	     ((:text-input label "Время"
			   value (or (@ transfer time) "")
			   error error
			   on-change (event-handler (v)
				       (setf (@ transfer time) v)
				       (chain this props
					      (on-change (@ this props value))))
			   class :time)))
	    ((:textarea-input label "Комментарий"
			      value (or (@ transfer comment) "")
			      on-change (event-handler (v)
					  (setf (@ transfer comment) v)
					  (chain this props
						 (on-change (@ this props value))))
			      class :comment)))))))))
