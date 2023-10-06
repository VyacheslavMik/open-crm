(in-package #:client.core)

(def-menu :root
  (category "Система")
  (item     "Пользователи" :root-user-page)

  (button   "Выйти"        logout :logout))

(def-menu :admin-hotel
  (category "Брони")
  (item     "Календарь бронирований" :calendar-page)
  (item     "Новое бронирование"     :new-booking-page)
  (item     "Все"                    :bookings-page)

  (category "Коммерческое предложение")
  (item     "Рассчет стоимости"      :calculate-offer-page selected)
  (item     "Отправленные"           :sent-offers-page)

  (category "Задачи")
  (item     "Все"  :admin-task-page)

  (category "Шаблоны")
  (item     "Коммерческое предложение (email)"    :offer-email-template-page)
  (item     "Счет на оплату (сайт)"               :invoice-site-template-page)
  (item     "Подтверждение оплаты (сайт)"         :receipt-site-template-page)
  (item     "Договор (сайт)"                      :agreement-site-template-page)

  (category "Настройки")
  (item     "Почта"                  :email-settings-page)
  (item     "Категории"              :categories-page)
  (item     "Номера"                 :hotel-room-page)
  (item     "Услуги"                 :service-page)
  (item     "Трансферы"              :transfer-page)
  (item     "Источники бронирований" :booking-source-page)
  (item     "Пользователи"           :admin-user-page)

  (category "Контекст")
  (item     "Отели" :hotel-page selected)

  (button   "Выйти"                  logout :logout))

(def-menu :admin-empty
  (category "Контекст")
  (item     "Отели" :hotel-page selected)

  (button   "Выйти" logout :logout))

(defun cleanerp ()
  (= (@ *user* role) "cleaner"))

(def-menu :driver
  (item   "Трансферы" :upcoming-transfers-page selected)
  (item   "Задачи"    :tasks-page)
  (button "Выйти"     logout :logout))

(def-menu :cleaner
  (item   "Трансферы" :upcoming-transfers-page selected)
  (item   "Задачи"    :tasks-page)
  (button "Выйти"     logout :logout))

(defvar *authorized-p*     nil)
(defvar *user*             nil)
(defvar *add-notification* nil)

(defvar *show-page*    nil)
(defvar *show-modal*   nil)
(defvar *close-modal*  nil)

(defvar *hotel-id*          nil)
(defvar *on-hotel-selected* nil)

(defun show-info (message)
  (funcall *add-notification* (create type    :info
				      message message)))

(defun show-error (message)
  (funcall *add-notification* (create type    :error
				      message message)))

(def-widget :notifications
  (ctor (props)
    (setf (@ this state) (create notifications #())))
  (component-did-mount ()
    (setf *add-notification* (event-handler (notification)
			       (chain this
				      (set-state
				       (lambda (state)
					 (chain state notifications (push notification))
					 state))))))
  (clear-notifications ()
    (chain this (set-state (create notifications #()))))
  (render ()
    (with-slots (notifications) (@ this state)
      (if (> (length notifications) 0)
	  ((:div class :notifications
		 on-click (@ this clear-notifications))
	   ((:div class (:notification :centered :column)
		  on-click (event-handler (e)
			     (chain e (stop-propagation))))
	    (foreach ((notification i) notifications)
	      ((:div class :message key i)
	       (when (eq (@ notification type) :error)
		 (:error-icon))
	       (:span
		(@ notification message))))
	    ((:div class :button)
	     ((:text-button on-click (@ this clear-notifications))
	      "Закрыть"))))
	  (return ())))))

(defun logout ()
  (setf *token*    nil)
  (setf *user*     nil)
  (setf *hotel-id* nil)

  (set-cookie :user     nil)
  (set-cookie :hotel-id nil)

  (apply *on-unauthorized*))

(defun section ()
  (let ((pathname (@ window location pathname)))
    (case pathname
      ("/about" :about)
      ("/manual" :manual)
      ("/feedback" :feedback)
      ("/app"
       (if *authorized-p*
	   :app-authorized
	   :app-anonymous)))))

(def-widget :app
  (ctor (props)
    (setf (@ this state) (create rerender-id 0
				 page nil
				 page-props nil
				 modal nil
				 modal-props nil))
    (setf *on-unauthorized* (http-handler ()
			      (setf *authorized-p* nil)
			      (chain this (set-state (create page nil)))
			      (chain this (on-authorized))))
    (setf *on-hotel-selected* (event-handler ()
				(chain this (on-hotel-selected))))
    (setf *show-page* (http-handler (page props)
			(chain this (set-state (create page page
						       page-props props)))))

    (setf *show-modal* (http-handler (modal props)
			 (chain this (set-state (create modal modal
							modal-props props)))))

    (setf *close-modal* (http-handler ()
			  (chain this (set-state (create modal nil
							 modal-props nil)))))

    (init-shared))

  (on-authorized ()
    (chain this (set-state (lambda (state)
			     (1+ (@ state rerender-id))))))

  (on-hotel-selected ()
    (chain this (set-state (lambda (state)
			     (1+ (@ state rerender-id))))))

  (render ()
    ((:div class :app)

     (:notifications)

     (case (section)
       (:about
	(:about-page))

       (:manual
	(:manual-page))

       (:feedback
	(:feedback-page))

       (:app-authorized
	((:div class :container)

	 (case (@ *user* role)
	   ("root"
	    (:root-menu))

	   ("admin"
	    (if *hotel-id*
		(:admin-hotel-menu)
		(:admin-empty-menu)))

	   ("driver"
	    (:driver-menu))

	   ("cleaner"
	    (:cleaner-menu)))

	 (when (@ this state page)
	   (dynamic-element
	    (find-react-component
	     (@ this state page))
	    (@ this state page-props)))

	 (when (@ this state modal)
	   ((:div id :modal)
	    ((:div class :modal-content)
	     (dynamic-element
	      (find-react-component
	       (@ this state modal))
	      (@ this state modal-props)))))))

       (:app-anonymous
	((:signin-page on-authorized (@ this on-authorized))))

       (t
	(:not-found-page))))))

(defun check-if-authorized (on-finish)
  (let ((user     (get-cookie :user))
	;; TODO: The logic behind hotel id is splitted by
	;; several files and it must be gathered in one place.
	(hotel-id (get-cookie :hotel-id)))

    (when hotel-id
      (setf *hotel-id* hotel-id))

    (if user
	(check-authorization
	 (create token (@ user token)
		 on-success (http-handler ()
			      (setf *token*        (@ user token))
			      (setf *user*         user)
			      (setf *hotel-id      hotel-id)
			      (setf *authorized-p* t))
		 on-error (http-handler ()
			    (set-cookie :user nil))
		 on-finish on-finish))
	(on-finish))))
 
(defun init ()
  (check-if-authorized
   (lambda ()
     (react-root :app))))
