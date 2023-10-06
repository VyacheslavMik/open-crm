(in-package #:date)

(defmacro month (d)
  `(chain ,d (get-month)))

(defsetf month (d) (v)
  `(chain ,d (set-month ,v)))

(defmacro date (d)
  `(chain ,d (get-date)))

(defsetf date (d) (v)
  `(chain ,d (set-date ,v)))

(defmacro year (d)
  `(chain ,d (get-full-year)))

(defsetf year (d) (v)
  `(chain ,d (set-full-year ,v)))

(eval-when (:compile-toplevel)

  (defpsmacro make-ymd (&key y m d)
    `(new (-date ,y ,m ,d)))

  (defpsmacro make-ms (value)
    `(new (-date ,value)))

  (defpsmacro now ()
    `(new (-date)))

  (defpsmacro incf-month (d &optional (v 1))
    `(make-ymd :y (year ,d) :m (+ (month ,d) ,v) :d (date ,d)))

  (defpsmacro incf-date (d &optional (v 1))
    `(make-ms (+ (chain ,d (get-time)) (* ,v 24 60 60 1000))))

  (defmacro def-date-op (name op)
    `(defpsmacro ,name (d1 d2 &rest ds)
       `(,',op ,@(mapcar (lambda (d)
			   `(chain ,d (get-time)))
			 (cons d1 (cons d2 ds))))))

  (def-date-op d= =)
  (def-date-op d> >)
  (def-date-op d>= >=)
  (def-date-op d< <)
  (def-date-op d<= <=)
  (def-date-op d/= /=))

(defun days-in-month (d)
  ;; Pay attention that :d 0 means that this is previous month last day
  (date (make-ymd :y (year d) :m (1+ (month d)) :d 0)))

(defun days-between (d1 d2)
  (unless (d>= d2 d1)
    (throw "d2 must be more than d1"))
  (parse-int (/ (- d2 d1)
		1000 60 60 24)
	     10))

(defun next-month-start (d)
  (make-ymd :y (year d) :m (1+ (month d)) :d 1))

(defun month-start (d)
  (make-ymd :y (year d) :m (month d) :d 1))

(defun days-in-year (d)
  (/ (- (make-ymd :y (1+ (year d)) :m 0 :d 1)
	(make-ymd :y (year d)      :m 0 :d 1))
     1000 60 60 24))

(defun now-date ()
  (let ((d (now)))
    (make-ymd :y (year d) :m (month d) :d (date d))))

(defun start-of-the-year ()
  (let ((d (now)))
    (setf (month d) 0)
    (setf (date d) 1)
    d))

(defun iso-date (d)  
  (let ((y (year d))
	(m (1+ (month d)))
	(d (date d)))
    (+ y "-" (if (< m 10) "0" "") m "-" (if (< d 10) "0" "") d)))

(defun unparse (type s)
  (case type
    (datetime
     (let ((datetime (parse-date s)))
       (+ (chain datetime (to-locale-date-string "ru-RU")) " "
	  (chain datetime (to-locale-time-string
			   "ru-RU"
			   (create hour "2-digit"
				   minute "2-digit"))))))

    (date
     (chain (parse-date s) (to-locale-date-string "ru-RU")))

    (otherwise
     (throw (+ "Unsupported type: " type)))))

(eval-when (:compile-toplevel)
  (defpsmacro parse-assertion (assertion message)
    `(unless ,assertion
       (if safe-p
    	   (return nil)
    	   (return ,message))))

  (defpsmacro parse-type-assertion ()
    `(parse-assertion (= (typeof s) "string")
		      (+ "Unsupported value type: " (typeof s) ", " s)))

  (defpsmacro parse-length-assertion (length)
    `(parse-assertion (= (@ s length) ,length)
		      (+ "Wrong date length: " s)))

  (defpsmacro parse-format-assertion (format)
    `(parse-assertion ,format
		      (+ "Wrong date format: " s))))
	 

(defun parse-iso (s &optional safe-p)
  (parse-type-assertion)
  (parse-length-assertion 10)

  (let ((year  (chain s (substring 0 4)))
	(dash1 (chain s (substring 4 5)))
	(month (chain s (substring 5 7)))
	(dash2 (chain s (substring 7 8)))
	(date  (chain s (substring 8 10))))

    (parse-format-assertion (and (= dash1 dash2 "-")
				 (number-p date)
				 (number-p month)
				 (number-p year)))

    (let ((year (parse-int year))
	  (month (parse-int month))
	  (date (parse-int date)))
      (make-ymd :y year
		:m (1- month)
		:d date))))

;; fd - stands for full-date
;; dm - stands for date-month
(defun iso-fd->date (s &optional safe-p)
  (parse-iso s safe-p))

;; dm stands for date-month
(defun iso-dm->date (s &optional safe-p)
  (parse-type-assertion)
  (parse-length-assertion 10)

  (let ((dashes (chain s (substring 0 5)))
	(month  (chain s (substring 5 7)))
	(dash   (chain s (substring 7 8)))
	(date   (chain s (substring 8 10))))

    (parse-format-assertion (and (= dash "-")
				 (= dashes "-----")
				 (number-p date)
				 (number-p month)))

    (let ((month (parse-int month))
	  (date (parse-int date)))
      (date:make-ymd :y 2002
		     :m (1- month)
		     :d date))))

(defun ru-fd->date (s &optional safe-p)
  (parse-type-assertion)
  (parse-length-assertion 5)

  (let ((date  (chain s (substring 0 2)))
	(dot   (chain s (substring 2 3)))
	(month (chain s (substring 3 5))))

    (parse-format-assertion (and (= dot ".")
				 (number-p date)
				 (number-p month)))

    (let ((month (parse-int month))
	  (date (parse-int date)))
      (date:make-ymd :y 2002
		     :m (1- month)
		     :d date))))

(defun ru-dm->date (s &optional safe-p)
  (parse-type-assertion)
  (parse-length-assertion 10)
  ;; TODO: Think about different lengths
  (when (and s (or (= (@ s length) 8)
		   (= (@ s length) 10)))
    (let ((date  (chain s (substring 0 2)))
	  (dot1  (chain s (substring 2 3)))
	  (month (chain s (substring 3 5)))
	  (dot2  (chain s (substring 5 6)))
	  (year  (chain s (substring 6 10))))
      (when (and (= dot1 dot2 ".")
		 (number-p date)
		 (number-p month)
		 (number-p year))
	(let ((year (parse-int year))
	      (month (parse-int month))
	      (date (parse-int date)))
	  (make-ymd :y (cond
			 ((> year 99) year)
			 ((> year 50) (+ 1900 year))
			 (t (+ 2000 year)))
		    :m (1- month)
		    :d date)))))
  (parse-type-assertion)
  (parse-length-assertion 5)

  (let ((date  (chain s (substring 0 2)))
	(dot   (chain s (substring 2 3)))
	(month (chain s (substring 3 5))))

    (parse-format-assertion (and (= dot ".")
				 (number-p date)
				 (number-p month)))

    (let ((month (parse-int month))
	  (date (parse-int date)))
      (date:make-ymd :y 2002
		     :m (1- month)
		     :d date))))

(defun unparse-iso (d)
  (let ((y (year d))
	(m (1+ (month d)))
	(d (date d)))
    (+ y "-" (if (< m 10) "0" "") m "-" (if (< d 10) "0" "") d)))

(defun try-parse (s)
  (when (and s (or (= (@ s length) 8)
		   (= (@ s length) 10)))
    (let ((date  (chain s (substring 0 2)))
	  (dot1  (chain s (substring 2 3)))
	  (month (chain s (substring 3 5)))
	  (dot2  (chain s (substring 5 6)))
	  (year  (chain s (substring 6 10))))
      (when (and (= dot1 dot2 ".")
		 (number-p date)
		 (number-p month)
		 (number-p year))
	(let ((year (parse-int year))
	      (month (parse-int month))
	      (date (parse-int date)))
	  (make-ymd :y (cond
			 ((> year 99) year)
			 ((> year 50) (+ 1900 year))
			 (t (+ 2000 year)))
		    :m (1- month)
		    :d date))))))

(defun parse-local-date (s)
  (unless s
    (throw "Date must be provided"))
  (new (-date (+ s "T00:00"))))
