(in-package #:client.core)

(def-widget :category-price-input
  (ctor ()
    (setf (@ this state) (create start-error nil
				 end-error nil
				 value-error nil
				 start ""
				 end ""
				 value "")))
  (render ()
    ((:div class (:category-price-input (@ this props class-name)))
     ((:label class (:activated
		     :label
		     (when (@ this props error)
		       :error)))
      (@ this props label))
     ((:div class (:input :column))
      ((:div class :new)
       ((:date-input label "Дата"
		     class :start
		     value (@ this state start)
		     error (@ this state start-error)
		     on-change (event-handler (v)
				 (chain this (set-state
					      (create start v
						      start-error nil))))
		     description "Начало"
		     type :date-month))
       ((:date-input label "Дата"
		     class :end
		     value (@ this state end)
		     error (@ this state end-error)
		     on-change (event-handler (v)
				 (chain this (set-state
					      (create end v
						      end-error nil))))
		     description "Окончание"
		     type :date-month))
       ((:integer-input label "Цена"
			class :value
			value (@ this state value)
			error (@ this state value-error)
			on-change (event-handler (v)
				    (chain this (set-state
						 (create value v
							 value-error nil))))
			description "Цена в этот период"))
       ((:div class :add-button)
	((:text-button class :add
		       on-click (event-handler ()
				  (block on-add
				    (let ((err (create)))
				      (unless (@ this state start)
					(setf (@ err start-error) "Обязательно"))
				      (unless (@ this state end)
					(setf (@ err end-error) "Обязательно"))
				      (unless (@ this state value)
					(setf (@ err value-error) "Обязательно"))
				      (when (and (@ this state start)
						 (@ this state end)
						 (> (@ this state start)
						    (@ this state end)))
					(setf (@ err start-error) "Начало должно быть меньше окончания"))
				      (when (and (@ this state value)
						 (< (@ this state value) 1))
					(setf (@ err value-error) "Цена должна быть больше нуля"))
				      (when (chain (or (@ this props value) #())
						   (some (chain
							  (lambda (item)
							    (or
							     (<= (@ item start)
								 (@ this state start)
								 (@ item end))
							     (<= (@ item start)
								 (@ this state end)
								 (@ item end))))
							  (bind this))))
					(setf (@ err start-error) "Интервал пересекается с существующим"
					      (@ err end-error)   "Интервал пересекается с существующим"))
				      (unless (empty-object-p err)
					(chain this (set-state err))
					(return-from on-add)))
				    (let ((value (or (@ this props value)
						     (array))))
				      (chain value (push (create start (@ this state start)
								 end   (@ this state end)
								 value (@ this state value))))
				      (chain value (sort (lambda (lhs rhs)
							   (cond
							     ((< (@ lhs start) (@ rhs start)) -1)
							     ((> (@ lhs start) (@ rhs start))  1)
							     (t 0)))))
				      (chain this (set-state (create start       ""
								     end         ""
								     value       ""
								     start-error ""
								     end-error   ""
								     value-error "")
							     (lambda ()
							       (chain this props (on-change value)))))))))
	 "+ Добавить")))
      (when (@ this props value)
	(foreach ((item i) (@ this props value))
	  ((:div key i class :item)
	   "С " (unparse-date (parse-date (@ item start))) " по "
	   (unparse-date (parse-date (@ item end))) " цена "
	   (@ item value) " рублей"
	   ((:text-button class :remove
			  on-click (event-handler ()
				     (chain this props
					    (on-change
					     (chain this props value
						    (filter (lambda (v)
							      (/= v item))))))))
	    "×")))))
     (when (@ this props error)
	 ((:div class :error-text) (@ this props error))))))
