(in-package #:client.core)

(defun group-rooms-by-category (rooms)
  (loop with res = (create)
	for room in rooms
	do (progn
	     (unless (getprop res (@ room category))
	       (setf (getprop res (@ room category)) #()))
	     (chain (getprop res (@ room category)) (push room)))
	finally (return res)))

(defun group-bookings-by-category (bookings)
  (loop with res = (create)
	for booking in bookings
	unless (@ booking room)
	do (progn
	     (unless (getprop res (@ booking room-category id))
	       (setf (getprop res (@ booking room-category id)) #()))
	     (chain (getprop res (@ booking room-category id)) (push booking)))
	finally (return res)))

;; TODO: Rewrite algorithm: map by day and booking from checkin to checkout
;;       starting from start and end. check-in < end, check-out > start
(defun group-bookings-by-room-and-day (bookings start end)
  (labels ((in-interval (check-in check-out)
	     (and (date:d< check-in end)
		  (date:d> check-out start))))
    (loop with res = (create)
	  for booking in bookings
	  for check-in = (date:parse-local-date (@ booking check-in))
	  for check-out = (date:parse-local-date (@ booking check-out))
	  when (and (@ booking room)
		    (in-interval check-in check-out))
	    do (progn
		 (unless (getprop res (@ booking room))
		   (setf (getprop res (@ booking room)) (create)))
		 (cond
		   ((date:d< check-in start)
		    (setf (getprop (getprop res (@ booking room)) (date:iso-date start))
			  (create value booking
				  colspan (days-between start check-out))))

		   ((date:d> check-out end)
		    (setf (getprop (getprop res (@ booking room)) (date:iso-date check-in))
			  (create value booking
				  colspan (1+ (days-between check-in end)))))

		   (t
		    (setf (getprop (getprop res (@ booking room)) (date:iso-date check-in))
			  (create value booking
				  colspan (days-between check-in check-out))))))
	  finally (return res))))

(defun group-bookings-by-id (bookings)
  (loop with res = (create)
	for booking in bookings
	do (setf (getprop res (@ booking id)) booking)
	finally (return res)))

(defun get-element-width (id)
  (let ((el (chain document (get-element-by-id id))))
    (if el
	(chain el (get-bounding-client-rect) width (to-string))
	"0")))

(def-view :month-row (props)
  (with-slots (start end) props
    (:tr
     ((:th class :frozen
	   id "month-row-first-cell"))
     ((:th class :frozen
	   style (:left (str (get-element-width "month-row-first-cell") "px"))
	   key start)
      (month-name start))
     (loop for d = (date:incf-date start) then (date:incf-date d)
	   collect (if (= (date:date d) 1)
		       ((:th class :frozen
			     style (:left (str (get-element-width "month-row-first-cell") "px"))
			     key d)
			(month-name d))
		       ((:th key d)))
	   until (date:d= d end)))))

(def-view :day-row (props)
  (with-slots (start end) props
    (:tr
     ((:td class (:frozen :calendar-date)))
     (loop for d = start then (date:incf-date d)
	   collect ((:td key d
			 class :calendar-date)
		    (date:date d))
	   until (date:d= d end)))))

(def-widget :day-cell
  (ctor ()
    (setf (@ this state) (create drag-overp nil)))
  (render ()
    ((:td on-drag-enter (event-handler (e)
			  (chain e (prevent-default))
			  (chain this (set-state (create drag-overp t))))
	  on-drag-leave (event-handler (e)
			  (chain e (prevent-default))
			  (chain this (set-state (create drag-overp nil))))
	  on-drag-over (event-handler (e)
			 (chain e (prevent-default)))
	  on-drop (event-handler (e)
		    (chain this (set-state (create drag-overp nil)))
		    (chain this props (save-booking
				       (chain e data-transfer (get-data "booking"))
				       (@ this props room)
				       (@ this props day)
				       (@ this props category))))
	  on-click (event-handler (e)
		     (goto-page :new-booking-page
				(create room-category-id (@ this props category id)
					room (@ this props room)
					check-in (date:unparse-iso (@ this props day)))))
	  class (:day-cell
		 (when (@ this state drag-overp)
		   :drag-over))))))

(defun booking-duration (booking)
  (let ((d (date:days-between (date:parse-local-date (@ booking check-in))
			      (date:parse-local-date (@ booking check-out)))))
    (if (> d 0) d 1)))

(def-view :category-booking-cell (props)
  ((:td class :category-booking
	draggable t
	on-drag-start (event-handler (e)
			(chain e data-transfer (set-data "booking" (@ props children id))))
	col-span (@ props colspan))
   (@ props children last-name) " "
   (@ props children first-name) " "
   (@ props children middle-name) " "))

(def-view :category-booking-row (props)
  (:tr
   ((:td class (:calendar-date :frozen)))
   ((:category-booking-cell)
    (@ props children))))

(def-view :room-row (props)
  (with-slots (start end) props
    (:tr
     ((:td class (:frozen :room))
      (@ props children name))
     (loop for d = start then (date:incf-date d)
     	   for booking = (let ((m (getprop (@ props room-and-day-bookings) (@ props children id))))
     			   (when m
     			     (getprop m (date:iso-date d))))
     	   if booking
     	     collect (progn
     		       (setf d (date:incf-date d (1- (@ booking colspan))))
     		       ((:category-booking-cell key (@ booking value id)
						colspan (@ booking colspan))
			(@ booking value)))
     	   else
     	     collect ((:day-cell key d
     				 day d
     				 room (@ props children id)
     				 category (@ props category)
     				 save-booking (@ props save-booking)))
     	   end
     	   until (date:d>= d end)))))

(def-view :category-row (props)
  (with-slots (start end) props
    (:<>
     ((:tr class :room-category)

      ((:td class (:frozen :category-cell))
       (@ props children name))

      (loop for i from 1 to (date:days-between start (date:incf-date end))
	    collect ((:td key i
			  class :category-cell))))

     (when (@ props category-bookings)
       (loop for booking in (@ props category-bookings)
	     collect ((:category-booking-row key (@ booking id)
					     start start
					     end end)
		      booking)))

     (when (@ props rooms)
       (loop for room in (@ props rooms)
	     collect ((:room-row key (@ room id)
				 start start
				 end end
				 save-booking (@ props save-booking)
				 category (@ props children)
				 room-and-day-bookings (@ props room-and-day-bookings))
		      room))))))

(def-page :calendar-page
  (ctor ()
    (setf (@ this state) (create hotel-room-status :loading
				 hotel-rooms #()
				 category-status :loading
				 categories #()
				 booking-status :loading
				 bookings #()
				 category-bookings #()
				 room-and-day-bookings #()
				 booking-array #()
				 start (date:incf-date (date:now-date) -10)
				 end (date:incf-date (date:now-date) 80))))
  (fetch-hotel-rooms ()
    (chain this (set-state (create hotel-room-status :loading)))
    (fetch-hotel-room
     (http-handler (v)
       (chain this (set-state (create hotel-rooms       (group-rooms-by-category v)
       				      hotel-room-status :loaded))))))
  (fetch-categories ()
    (chain this (set-state (create category-status :loading)))
    (fetch-category
     (http-handler (v)
       (chain this (set-state (create categories      v
				      category-status :loaded))))))
  (fetch-bookings ()
    (chain this (set-state (create booking-status :loading)))
    (fetch-booking
     (http-handler (v)
       (with-slots (start end) (@ this state)
	 (chain this (set-state (create category-bookings     (group-bookings-by-category v)
					room-and-day-bookings (group-bookings-by-room-and-day v
											      start
											      end)
					booking-array         v
					bookings              (group-bookings-by-id v)
					booking-status        :loaded)))))))

  (save-booking (id room date category)
    (let ((booking (getprop (@ this state bookings) id)))
      (when (@ booking room)
	(let ((d (booking-duration booking)))
	  (setf (@ booking check-in) (date:iso-date date))
	  (setf (@ booking check-out) (date:iso-date (date:incf-date date d)))))
      (setf (@ booking room) room)
      (setf (@ booking room-category) category)
      (setf (@ booking room-category-id) (@ category id))
      (update-booking booking (http-handler (e)
				(chain this (fetch-bookings))))))

  (recalculate-bookings (start end)
    (with-slots (booking-array) (@ this state)
      (chain this (set-state
		   (create start start
			   end end
			   room-and-day-bookings (group-bookings-by-room-and-day booking-array
										 start
										 end))))))
  (component-did-mount ()
    (chain this (fetch-hotel-rooms))
    (chain this (fetch-categories))
    (chain this (fetch-bookings)))

  (render ()
    (with-slots (start end) (@ this state)
      ((:div class :calendar-page)
       ((:div class :interval)
	((:date-input label "Начало интервала"
		      value (date:unparse-iso start)
		      on-change (event-handler (v)
				  (chain this (recalculate-bookings (date:parse-local-date v)
								    (@ this state end))))
		      type :full-date
		      class :date))
	((:div class :separator))
	((:date-input label "Окончание интервала"
		      value (date:unparse-iso end)
		      on-change (event-handler (v)
				  (chain this (recalculate-bookings (@ this state start)
								    (date:parse-local-date v))))
		      type :full-date
		      class :date)))
       ((:div class :calendar-container)
       	(:table
       	 (:tbody
       	  ((:month-row start start
       		       end   end))
       	  ((:day-row start start
       		     end   end))
       	  (loop for category in (@ this state categories)
       		collect ((:category-row key (@ category id)
       					start start
       					end end
       					category-bookings (getprop
       							   (@ this state category-bookings)
       							   (@ category id))
       					room-and-day-bookings (@ this state room-and-day-bookings)
       					save-booking (@ this save-booking)
       					rooms (getprop (@ this state hotel-rooms) (@ category id)))
       			 category)))))))))
