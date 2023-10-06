(in-package #:client.core)

(eval-when (:compile-toplevel)
  (defpsmacro def-button (name)
    `(def-view ,name (props)
       (with-slots (on-click class-name children) props
	 ((:button class (,name class-name)
		    on-click (event-handler (e)
			       (chain e (stop-propagation))
			       (chain e (prevent-default))
			       (chain props (on-click e)))
		    type :submit)
	   children)))))

(def-button :contained-button)
(def-button :outlined-button)
(def-button :text-button)
