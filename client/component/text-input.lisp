(in-package #:client.core)

(def-widget :text-input
  (ctor ()
    (setf (@ this input) (chain -react (create-ref)))
    (setf (@ this state) (create error nil
				 focused nil)))

  (component-did-update ()
    (when (and (@ this input current) (@ this cursor))
      (chain this input current
	     (set-selection-range (@ this cursor) (@ this cursor)))))

  (render ()
    ((:div class (:text-input (@ this props class-name)))
     ((:label class (:label
		     (cond
		       ((@ this state focused)
			:focused)

		       ((@ this props value)
			:activated))
		     (when (and (@ this props error)
				(or (@ this state focused)
				    (@ this props value)))
		       :label-error))
	      on-click (event-handler (e)
			 (chain this input current (focus))))
      (@ this props label))
     ((:input on-blur (event-handler (e)
			(chain this (set-state (create focused nil))))
	      on-focus (event-handler (e)
			 (chain this (set-state (create focused t))))
	      read-only (@ this props read-only)
	      on-change (unless (@ this props read-only)
			  (event-handler (e)
			    (setf (@ this cursor) (@ e target selection-start))
			    (chain this props (on-change (@ e target value)))))
	      class (cond
		      ((@ this props error)   :input-error)
		      ((@ this state focused) :focused))
	      type (or (@ this props type) :text)
	      ref (@ this input)
	      value (@ this props value)))
     (if (@ this props error)
	 ((:div class :error-text)  (@ this props error))
	 ((:div class :description) (@ this props description))))))
