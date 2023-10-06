(in-package #:client.core)

(def-widget :select-input
  (ctor ()
    (setf (@ this body-on-click) (event-handler (e)
				   (when (chain this (stop-editing-p e))
				     (chain this (set-state (create editing-p nil)))
				     (chain document body
					    (remove-event-listener
					     "click"
					     (@ this body-on-click)))))
	  (@ this items)     (chain -react (create-ref))
	  (@ this div-input) (chain -react (create-ref))
	  (@ this label)     (chain -react (create-ref)))
    (setf (@ this state) (create error nil
				 editing-p nil)))

  (stop-editing-p (e)
    (and (@ this state editing-p)
	 (not (clicked-inside-p (@ this div-input) e))
	 (not (clicked-inside-p (@ this items) e))
	 (not (clicked-inside-p (@ this label) e))))

  (maybe-start-edit (e)
    (unless (@ this state editing-p)
      (unless (clicked-inside-p (@ this items) e)
	(chain this (set-state (create editing-p t)
			       (lambda ()
				 (chain document body
					(add-event-listener
					 "click"
					 (@ this body-on-click)))))))))

  (component-will-unmount ()
    (chain document body
	   (remove-event-listener
	    "click"
	    (@ this body-on-click))))

  (show-value ()
    (when (and (@ this props value)
	       (@ this props items))
      (let* ((value (@ this props value))
	     (item (chain this props items
			  (find (lambda (v) (= (@ v key) value))))))
	(when item
	  (react-element
	   ((:div class :value)
	    (@ item label)))))))

  (render ()
    ((:div class (:select-input (@ this props class-name)))

     ((:div class (:input
		   (cond
		     ((@ this props error)     :input-error)
		     ((@ this state editing-p) :focused)))
	    ref (@ this div-input)
	    on-click (@ this maybe-start-edit))

      (when (@ this state editing-p)
	(chain this (show-value)))

      (if (@ this state editing-p)
	  ((:div class :items
		 ref (@ this items))
	   (with-slots (items no-elements-text) (@ this props)
	     (if (emptyp items)
		 ((:div class (:empty-value :item)
			on-click (event-handler (e)
				   (chain document body
					  (remove-event-listener
					   "click"
					   (@ this body-on-click)))
				   (chain this (set-state (create editing-p nil)))))
		  (or no-elements-text "Нет элементов"))
		 (foreach (item items)
		   ((:div class :item
			  key (@ item key)
			  on-click (event-handler (e)
				     (when (@ this props validators)
				       (let (error)
					 (loop for v in (@ this props validators)
					       do (let ((err (v item)))
						    (when err
						      (setf error err))))
					 (chain this (set-state (create error error)))))
				     (chain document body
					    (remove-event-listener
					     "click"
					     (@ this body-on-click)))
				     (chain this (set-state
						  (create editing-p nil)
						  (lambda ()
						    (chain this props
							   (on-change (@ item key))))))))
		    (@ item label))))))
	  (chain this (show-value)))

      ((:div class :down-arrow)
       (:i)))

     ((:label class (:label
		     (cond
		       ((@ this state editing-p) :focused)
		       ((@ this props value)     :activated))
		     (when (and (@ this props error)
				(or (@ this state editing-p)
				    (@ this props value)))
		       :label-error))
	      on-click (@ this maybe-start-edit)
	      ref (@ this label))
      (@ this props label))

     (if (@ this props error)
	 ((:div class :error-text)  (@ this props error))
	 ((:div class :description) (@ this props description))))))
