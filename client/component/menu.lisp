(in-package #:client.core)

(def-view :menu-category (props)
  (with-slots (id title class-name toggledp) props
    ((:div class (:menu-category class-name)
	   on-click (event-handler (e)
		      (chain props (on-click id))))

     (:span title)

     ((:div class :icon)
      (if toggledp
	  ((:up-arrow-icon))
	  ((:down-arrow-icon)))))))

(def-view :menu-item (props)
  (with-slots (id title class-name showedp) props
    (if showedp
	((:div class (:menu-item class-name)
	       on-click (lambda (e)
			  (chain props (on-click id))))
	 title)
	(return ()))))

(def-view :menu-button (props)
  (with-slots (title class-name on-click) props
    ((:div class (:menu-item class-name)
	   on-click on-click)
     title)))

(eval-when (:compile-toplevel)
  (defpsmacro def-menu (name &rest items)
    (let* (selected-item
	   first-item
	   current-category
	   mapping
	   (items (mapcar (lambda (item)
			    (ecase (car item)
			      (category
			       (let ((id (symbol-to-js-string (gensym))))
                                 (setf current-category id)
                                 `((:menu-category id        ,id
						   title     ,(cadr item)
						   class-name (chain this (get-category-class-name ,id))
						   toggledp   (chain this (get-category-toggle-state ,id))
						   on-click   (@ this on-category-click)))))

			      (item
			       (let ((id (symbol-to-js-string (gensym)))
				     (page (caddr item)))
				 (unless first-item
				   (setf first-item id))
				 (when (some (lambda (v) (eq v 'selected)) item)
				   (setf selected-item id))
				 (push `(create category-id ,current-category
						page ,page)
				       mapping)
				 (push id mapping)
				 (push id mapping)
				 (push page mapping)
				 `((:menu-item id                   ,id
					       title                ,(cadr item)
					       class-name            (chain this (get-item-class-name ,id))
					       showedp               (chain this (get-item-show-state ,id))
					       on-click              (@ this on-item-click)))))

			      (button
			       `((:menu-button title      ,(cadr item)
					       class-name ,(cadddr item)
					       on-click   ,(caddr item))))))
			  items)))
      `(def-widget ,(make-keyword (format nil "~(~a~)-menu" name))
	 (ctor ()
	   (let ((cstate (array)))
	     (setf (@ this mapping) (create ,@mapping)
		   (@ this state)   (create selected-item nil
					    toggle-state (create)))))

	 (component-did-mount ()
	   (let* ((stored (local-storage/restore :selected-menu-item))
		  (id (getprop this 'mapping stored)))
	     (if id
		 (chain this (on-item-click id))
		 (chain this (on-item-click ,(or selected-item first-item))))))

	 (get-category-id (item-id)
	   (when item-id
	     (getprop this 'mapping item-id 'category-id)))

	 (get-page (item-id)
	   (when item-id
	     (getprop this 'mapping item-id 'page)))

	 (get-category-class-name (id)
	   (with-slots (selected-item) (@ this state)
	     (let ((category-id (chain this (get-category-id selected-item))))
	       (when (= category-id id)
		 :selected))))

	 (get-category-toggle-state (id)
	   (with-slots (toggle-state) (@ this state)
	     (getprop toggle-state id)))

	 (get-item-class-name (id)
	   (when (= id (@ this state selected-item))
	     :selected))

	 (get-item-show-state (id)
	   (with-slots (toggle-state) (@ this state)
	     (let ((category-id (chain this (get-category-id id))))
	       (getprop toggle-state category-id))))

	 (on-category-click (id)
	   (let ((old (@ this state toggle-state)))
	     (setf (getprop old id) (not (getprop old id)))
	     (chain this (set-state (create toggle-state old)))))

	 (on-item-click (id)
	   (let ((toggle-state (@ this state toggle-state))
		 (category-id (chain this (get-category-id id)))
		 (page (chain this (get-page id))))
	     (unless (getprop toggle-state category-id)
	       (setf (getprop toggle-state category-id) t))
	     (chain this (set-state (create selected-item id
					    toggle-state toggle-state)))
	     (local-storage/store :selected-menu-item page)
	     (goto-page page)))

	 (render ()
	   ((:div class :menu)
	    ,@items))))))
