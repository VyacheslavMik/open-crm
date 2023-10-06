(in-package #:client.core)

(defun to-number (v)
  (cond
    ((= v "-") v)
    (v (+ v))
    (t "")))

(def-widget :integer-input
  (ctor ()
    (setf (@ this input) (chain -react (create-ref)))
    (setf (@ this state) (create error nil
				 focused nil)))
  (render ()
    ((:div class (:integer-input (@ this props class-name)))
     ((:label class (:label
		     (cond
		       ((@ this state focused)
			:focused)
		       ((or (= (@ this props value) 0)
			    (@ this props value))
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
	      on-change (event-handler (e)
			  (let ((v (@ e target value)))
			    (when (number-p v)
			      (chain this props (on-change (to-number v))))))
	      class (cond
		      ((@ this props error)   :input-error)
		      ((@ this state focused) :focused))
	      ref (@ this input)
	      value (@ this props value)))
     (if (@ this props error)
	 ((:div class :error-text)  (@ this props error))
	 ((:div class :description) (@ this props description))))))