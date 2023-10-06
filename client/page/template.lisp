(in-package #:client.core)

(eval-when (:compile-toplevel)
  (defpsmacro def-template-page (header entity type)
    (let ((form-name (make-keyword (format nil "~:@(~a-~a~a~)" entity type "-template-form")))
	  (page-name (make-keyword (format nil "~:@(~a-~a~a~)" entity type "-template-page"))))
      `(progn
	 (def-form ,form-name
	   (submit
	    (post-template
	     ,entity ,type
	     (@ this state form-value)
	     (lambda (v)
	       (show-info "Данные сохранены"))))
	   (render
	    ((:div class (:form :template-form))
	     ,(when (eq type :email)
		`(form-field :text-input
			     (subject)
			     ((lambda (v) (unless v "Поле обязательно")))
			     (label "Тема"
			      description "Введите тему письма")))
	     (form-field :textarea-input
			 (value)
			 ((lambda (v) (unless v "Поле обязательно")))
			 (label "Шаблон"
			  description "Задайте шаблон"))
	     ((:contained-button class :submit-button
				 on-click (@ this submit))
	      "Сохранить"))))

	 (def-page ,page-name
	   (ctor ()
	     (setf (@ this state) (create status :loading
					  form-value nil)))
	   (component-did-mount ()
	     (chain this (set-state (create status :loading)))
	     (fetch-template
	      ,entity ,type
	      (http-handler (v)
		(chain this (set-state (create form-value (if (empty-object-p v)
							      (create subject ""
								      value   "")
							      v)
					       status :loaded))))))
	   (render ()
	     ((:div class (:column :page))
	      ((:div class :header)
	       (:h3 ,header))
	      (if (= (@ this state status) :loading)
		  (:div "Загрузка")
		  ((,form-name form-value (@ this state form-value)))))))))))

(def-template-page "Шаблон коммерческого предложения (email)"    :offer :email)
(def-template-page "Шаблон счета на оплату"                      :invoice :site)
(def-template-page "Шаблон подтверждения оплаты"                 :receipt :site)
(def-template-page "Шаблон договора"                             :agreement :site)
