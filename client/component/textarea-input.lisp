(in-package #:client.core)

(def-widget :textarea-input
  (ctor ()
    (setf (@ this input) (chain -react (create-ref)))
    (setf (@ this state) (create error nil
				 focused nil)))
  (component-did-update ()
    (when (and (@ this input current) (@ this cursor))
      (chain this input current
	     (set-selection-range (@ this cursor) (@ this cursor)))))
  (render ()
    ((:div class (:textarea-input (@ this props class-name)))
     ((:label class (:label
		     (if (@ this state focused)
			 :focused
			 :activated)
		     (when (@ this props error)
		       :label-error)))
      (@ this props label))
     ((:textarea on-blur (event-handler ()
			   (chain this (set-state (create focused nil))))
		 on-focus (event-handler ()
			    (chain this (set-state (create focused t))))
		 on-change (event-handler (e)
			     (setf (@ this cursor) (@ e target selection-start))
			     (chain this props (on-change (@ e target value))))
		 class-name (cond
			      ((@ this props error)   :input-error)
			      ((@ this state focused) :focused))
		 ref (@ this input)
		 value (@ this props value)))
     (if (@ this props error)
	 ((:div class :error-text)  (@ this props error))
	 ((:div class :description) (@ this props description))))))
