(in-package #:client.core)

;; TODO: Remove this clunky date parsing.
(defun parse-date (s)
  (if s
      (new (-date s))
      (new (-date))))

(defvar months #("Январь"  "Февраль" "Март"
		 "Апрель"  "Май"     "Июнь"
		 "Июль"    "Август"  "Сентябрь"
		 "Октябрь" "Ноябрь"  "Декабрь"))

(defun month-name (d)
  (when d
    (getprop months (date:month d))))

(defun unparse-date (date)
  (when date
    (date:unparse 'date date)))

(defun next-month-p (lhs rhs)
  (let ((d (new (-date rhs))))
    (chain d (set-date (+ (chain d (get-date)) 1)))
    (= (chain lhs (get-month)) (chain d (get-month)))))

(defun next-day (date)
  (chain date (set-date (+ (chain date (get-date)) 1)))
  date)

(defun prev-day (date)
  (chain date (set-date (- (chain date (get-date)) 1)))
  date)

(defun prepare-days (lower-limit upper-limit date)
  (if date
      (let* ((day (date:month-start date))
	     (lower-limit (when lower-limit (date:parse-iso lower-limit)))
	     (upper-limit (when upper-limit (date:parse-iso upper-limit)))
	     started-p)
	(loop
	  while (next-month-p date day)
	  collect (loop for d from 1 to 7
			do (if started-p
			       (next-day day)
			       (setf started-p (and (= (chain day (get-day))
						       (if (= d 7)
							   0
							   d)))))
			collect (if (and started-p
					 (= (chain day (get-month))
					    (chain date (get-month)))
					 (or (not lower-limit)
					     (< lower-limit day))
					 (or (not upper-limit)
					     (< day upper-limit)))
				    (chain day (get-date))
				    ""))))
      #()))

(defun today-p (date month year)
  (let ((today (date:now)))
    (and (= year (date:year today))
	 (= month (date:month today))
	 (= date (date:date today)))))

(defun selected-p (value date month year type)
  (when value
    (let ((d (case type
	       (:date-month
		(date:iso-dm->date value))
	       (:full-date
		(date:iso-fd->date value)))))
      (and (= year (date:year d))
	   (= month (date:month d))
	   (= date (date:date d))))))

(defun date-input/try-parse-input (s)
  (unless (= (typeof s) "string")
    (return nil))

  (unless (= (@ s length) 5)
    (return nil))

  (let ((date  (chain s (substring 0 2)))
	(dot   (chain s (substring 2 3)))
	(month (chain s (substring 3 5))))

    (unless (and (= dot ".")
		 (number-p date)
		 (number-p month))
      (return nil))

    (let ((month (parse-int month))
	  (date (parse-int date)))
      (date:make-ymd :y date-input/default-year
		     :m (1- month)
		     :d date))))

(defun date-input/unparse-iso-date-month (d)
  (let ((m (1+ (date:month d)))
	(d (date:date d)))
    (+ "-----" (if (< m 10) "0" "") m "-" (if (< d 10) "0" "") d)))

(def-widget :date-input
  (ctor (props)
    (let ((new-state (chain this (parse-value props))))
      (setf (@ this body-on-click) (event-handler (e)
				     (when (chain this (stop-editing-p e))
				       (chain this (set-state (create editing-p nil
								      input-value nil)))
				       (chain document
					      body
					      (remove-event-listener
					       "click"
					       (@ this body-on-click)))))
	    (@ this calendar)  (chain -react (create-ref))
	    (@ this div-input) (chain -react (create-ref))
	    (@ this label)     (chain -react (create-ref))
	    (@ this input)     (chain -react (create-ref)))
      (setf (@ this state) (create error nil
				   editing-p nil
				   input-value ""
				   input-class ""
				   calendar-class ""

				   parsed (@ new-state parsed)
				   value (@ props value)

				   year (@ new-state year)
				   month (@ new-state month)
				   date (@ new-state date)))))

  (stop-editing-p (e)
    (and (@ this state editing-p)
	 (not (clicked-inside-p (@ this div-input) e))
	 (not (clicked-inside-p (@ this label) e))))

  (parse-value (props)
    (with-slots (value type) props
      (if value
	  (let ((res (case type
		       (:date-month (date:iso-dm->date value))
		       (:full-date (date:iso-fd->date value)))))
	    (if (= (typeof res) "string")
		(throw res)
		(let ((year (date:year res))
		      (month (date:month res))
		      (date (date:date res)))
		  (create month month
			  year year
			  date date
			  parsed (case type
				   (:date-month
				    (let ((d (date:date res))
					  (m (date:month res)))
				      (+ (if (< d 10) "0" "") d "." (if (< m 10) "0" "") m)))
				   (:full-date
				    (date:unparse 'date res)))))))
	  (let* ((d (date:now))
		 (year (date:year d))
		 (month (date:month d))
		 (date (date:date d)))
	    (create month month
		    year year
		    date date)))))

  (calculate-calendar-and-input-classes (calendar div-input)
    (if (and (@ calendar  current)
	     (@ div-input current))
	(if (> (@ calendar  current offset-width)
	       (@ div-input current offset-width))
	    (create input-class    :narrow
		    calendar-class :wide)
	    (create input-class    :wide
		    calendar-class :narrow))
	(create input-class (@ this state input-class)
		calendar-class (@ this state calendar-class))))

  (change-month (dir)
    (chain this (set-state (lambda (state)
			     (with-slots (month year date) state
			       (let* ((d (date:make-ymd :y year :m month :d date))
				      (d (date:incf-month d (case dir
							      (:next 1)
							      (:prev -1)))))
				 (create month (date:month d)
					 year (date:year d))))))))

  (next-month ()
    (chain this (change-month :next)))

  (prev-month ()
    (chain this (change-month :prev)))

  (on-change (iso-date)
    (chain this props (on-change iso-date)))

  (select-date (date)
    (chain document
	   body
	   (remove-event-listener
	    "click"
	    (@ this body-on-click)))
    (with-slots (year month) (@ this state)
      (let ((d (date:make-ymd :y year :m month :d date)))
	(chain this (set-state (create editing-p nil
				       input-value nil)
			       (lambda ()
				 (chain this (on-change
					      (case (@ this props type)
						(:date-month
						 (date-input/unparse-iso-date-month d))
						(:full-date
						 (date:unparse-iso d)))))))))))

  (get-date ()
    (with-slots (year month date) (@ this state)
      (date:make-ymd :y year :m month :d date)))

  (lowest-month-p ()
    (with-slots (lower-limit type) (@ this props)
      (let ((current (chain this (get-date))))
	(case type
	  (:date-month
	   (when current
	     (= (date:month current) 0)))
	  (:full-date
	   (when (and lower-limit current)
	     (let ((d-limit (date:parse-iso lower-limit)))
	       (and (<= (date:year current)
			(date:year d-limit))
		    (<= (date:month current)
			(date:month d-limit))))))))))

  (uppermost-month-p ()
    (with-slots (upper-limit type) (@ this props)
      (let ((current (chain this (get-date))))
	(case type
	  (:date-month
	   (when current
	     (= (date:month current) 11)))
	  (:full-date
	   (when (and upper-limit current)
	     (let ((d-limit (date:parse-iso upper-limit)))
	       (and (>= (date:year current)
			(date:year d-limit))
		    (>= (date:month current)
			(date:month d-limit))))))))))

  (component-did-update (prev-props)
    (let ((new-state (chain this (calculate-calendar-and-input-classes
				  (@ this calendar)
				  (@ this div-input)))))
      (unless (= (@ this state value)
		 (@ this props value))
	(let ((parsed-state (chain this (parse-value (@ this props)))))
	  (setf (@ new-state value) (@ this props value)
		(@ new-state parsed) (@ parsed-state parsed)
		(@ new-state year) (@ parsed-state year)
		(@ new-state month) (@ parsed-state month)
		(@ new-state date) (@ parsed-state date))))
      (when (or (/= (@ this state value) (@ this props value))
		(/= (@ new-state input-class) (@ this state input-class))
		(/= (@ new-state calendar-class) (@ this state calendar-class)))
	(chain this (set-state new-state)))))

  (maybe-start-edit (e)
    (unless (@ this state editing-p)
      (unless (clicked-inside-p (@ this calendar) e)
	(chain this (set-state (create editing-p t)
			       (lambda ()
				 (when (@ this input current)
				   (chain this input current (focus)))
				 (chain document body
					(add-event-listener
					 "click"
					 (@ this body-on-click)))))))))

  (component-will-unmount ()
    (chain document body
	   (remove-event-listener
	    "click"
	    (@ this body-on-click))))

  (render ()
    ((:div class (:date-input
		  (@ this props class-name)
		  (when (@ this state editing-p)
		    :focused)))

     ((:div class (:input
		   (@ this state input-class)
		   (when (@ this props error)
		     :input-error)
		   (when (@ this state editing-p)
		     :focused))
	    on-click (event-handler (e)
		       (chain this (maybe-start-edit e)))
	    ref (@ this div-input))

      ((:input class :value
	       value (cond
		       ((@ this state input-value)
			(@ this state input-value))
		       
		       ((@ this state parsed)
			(@ this state parsed))

		       (t
			""))
	       on-change (event-handler (e)
			   (let* ((s (@ e target value))
				  (d (case (@ this props type)
				       (:date-month
					(date-input/try-parse-input s))
				       (:full-date
					(date:try-parse s))))
				  (new-state (create input-value (@ e target value))))

			     (when d
			       (setf (@ new-state year)  (date:year d)
				     (@ new-state month) (date:month d)
				     (@ new-state date)  (date:date d))
			       (chain this (on-change
					    (case (@ this props type)
					      (:date-month
					       (date-input/unparse-iso-date-month d))
					      (:full-date
					       (date:unparse-iso d))))))

			     (chain this (set-state new-state))))
	       ref (@ this input)
	       read-only (not (@ this state editing-p))))

      (when (@ this state editing-p)
	((:div class (:calendar
		      (@ this state calendar-class)
		      (when (@ this props error)
			:input-error))
	       ref (@ this calendar))
	 ((:div class :calendar-header)
	  ((:div class (:arrow-left
			(when (chain this (lowest-month-p))
			  :hidden))
		 on-click (@ this prev-month))
	   (:i))
	  ((:div class :month-year)
	   ((:div class :month) (getprop months (@ this state month)))
	   (when (= (@ this props type) :full-date)
	     ((:div class :year) (@ this state year))))
	  ((:div class (:arrow-right
			(when (chain this (uppermost-month-p))
			  :hidden))
		 on-click (@ this next-month))
	   (:i)))
	 ((:table class :days)
	  (:thead
	   (:tr
	    (foreach (week-day '("Пн" "Вт" "Ср" "Чт" "Пт" "Сб" "Вс"))
	      ((:td class :week-day
		    key week-day)
	       week-day))))
	  (:tbody
	   (with-slots (month year) (@ this state)
	     (let ((date        (chain this (get-date)))
		   (lower-limit (@ this props lower-limit))
		   (upper-limit (@ this props upper-limit)))
	       (foreach ((week i) (prepare-days lower-limit upper-limit date))
		 ((:tr class :week key i)
		  (foreach ((day j) week)
		    ((:td class (:day
				 (when day
				   :not-empty)
				 (when (today-p day month year)
				   :current)
				 (when (selected-p (@ this props value)
						   day
						   month
						   year
						   (@ this props type))
				   :selected))
			  on-click (event-handler (e)
				     (when day
				       (chain this (select-date day))))
			  key j)
		     day)))))))))))
     ((:label class (:label
		     (cond
		       ((@ this state editing-p) :focused)
		       ((@ this props value)     :activated))
		     (when (and (@ this props error)
				(or (@ this state editing-p)
				    (@ this props value)))
		       :label-error))
	      ref (@ this label)
	      on-click (event-handler (e)
			 (chain this (maybe-start-edit e))))
      (@ this props label))
     (if (@ this props error)
	 ((:div class :error-text)  (@ this props error))
	 ((:div class :description) (@ this props description))))))
