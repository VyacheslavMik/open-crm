(in-package #:client.core)

(def-form :task-form
  (submit
   (unless (@ this state form-value id)
     (setf (@ this state form-value recorded) (parse-date)))
   (if (@ this state form-value id)
       (update-task (@ this state form-value)
		    (@ this props on-save))
       (new-task    (@ this state form-value)
		    (@ this props on-save))))
  (render
   ((:div class (:form :task-form))
    ((:div class :column)
     (form-field :select-input
		 (user)
		 ((lambda (v) (unless v "Поле обязательно")))
		 (label "Пользователь"
		  description "Выберите пользователя ответственного за задачу"
		  items (@ this props users)))
     (:div
      (form-field :date-input
		  (date)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Дата"
		   description "Дата окончания"
		   class :date
		   type :full-date))
      (form-field :text-input
		  (time)
		  ((lambda (v) (unless v "Поле обязательно")))
		  (label "Время"
		   description "Время окончания")))
     (form-field :text-input
		 (subject)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Тема"
		  description "Введите тему"))
     (form-field :textarea-input
		 (description)
		 ((lambda (v) (unless v "Это обязательное поле")))
		 (label "Описание"
		  description "Введите описание"
		  class :description)))
    ((:div class :buttons)
     ((:contained-button class :submit-button
			 on-click (@ this submit))
      "Сохранить")
     ((:outlined-button class :close-button
			on-click (@ this props on-close))
      "Закрыть")))))

(defun role-display (user)
  (case (@ user role)
    ("admin"   "Администратор")
    ("driver"  "Водитель")
    ("cleaner" "Уборщица")
    (otherwise "Неизвестно")))

(defun user-display (user)
  (+ (@ user login) " (" (role-display user) ")"))

(defun deadline-fail-p (datetime)
  (> (date:now) (parse-date datetime)))

(def-view :task-view (props)
  (let ((user-map (@ props user-map)))
    (with-slots (recorded date time user subject description) (@ props children)
      ((:div class (:task-view :column (when user-map :admin)))
       ((:span class :recorded)
	(date:unparse 'datetime recorded))
       ((:span class (when (deadline-fail-p (+ date "T" time))
		       :deadline-fail))
	(:b "Выполнить до: ") 
	(date:unparse 'datetime (+ date "T" time)))
       (when user-map
	 (:span
	  (:b "Назначено на: ")
	  (user-display (getprop user-map user))))
       (:h3 subject)
       (:span description)))))

(def-page :admin-task-page
  (ctor ()
    (setf (@ this state) (create task-status :loading
				 tasks #()
				 user-status :loading
				 users #()
				 show-form-p nil
				 form-value nil)))
  (fetch-tasks ()
    (chain this (set-state (create task-status :loading)))
    (fetch-task
     (http-handler (v)
       (chain this (set-state (create tasks v
				      task-status :loaded))))))
  (fetch-users ()
    (chain this (set-state (create user-status :loading)))
    (fetch-hotel-users
     (http-handler (v)
       (chain this (set-state
		    (create users       (map (lambda (user)
					       (create key   (@ user id)
						       label (user-display user)))
					     v)
			    user-map    (as-hash-map v)
			    user-status :loaded))))))
  (on-close ()
    (chain this (set-state (create show-form-p nil
				   form-value nil))))
  (on-save ()
    (chain this (set-state (create show-form-p nil
				   form-value nil)))
    (chain this (fetch-tasks)))
  (component-did-mount ()
    (chain this (fetch-tasks))
    (chain this (fetch-users)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Задачи")
      ((:text-button on-click (event-handler ()
				(chain this (set-state (create show-form-p t)))))
       "+ Добавить"))
     (cond
       ((or (= (@ this state task-status) :loading)
	    (= (@ this state user-status) :loading))
	(:div "Загрузка"))
       ((@ this state show-form-p)
	((:task-form on-close   (event-handler ()
				  (chain this (set-state (create show-form-p nil
								 form-value nil))))
		     users      (@ this state users)
		     form-value (@ this state form-value)
		     on-save    (@ this on-save))))
       (t
	(foreach (task (@ this state tasks))
	  ((:div class (:entity :column :task)
		 on-click (event-handler ()
			    (chain this (set-state (create show-form-p t
							   form-value task))))
		 key (@ task id))
	   ((:task-view user-map (@ this state user-map))
	    task)
	   ((:text-button class :remove
			  on-click (event-handler (e)
				     (chain e (stop-propagation))
				     (delete-task task
						  (@ this fetch-tasks))))
	    "Удалить"))))))))

(def-page :tasks-page
  (ctor ()
    (setf (@ this state) (create status :loading
				 tasks #())))
  (fetch-tasks ()
    (chain this (set-state (create task-status :loading)))
    (get-my-tasks
     (http-handler (v)
       (chain this (set-state (create tasks v
				      status :loaded))))))
  (component-did-mount ()
    (chain this (fetch-tasks)))
  (render ()
    ((:div class (:column :page :entities-page))
     ((:div class :header)
      (:h3 "Задачи"))
     (if (= (@ this state status) :loading)
	 (:div "Загрузка")
	 (foreach (task (@ this state tasks))
	   ((:div class (:entity :column :task) key (@ task id))
	    (:task-view task)))))))
