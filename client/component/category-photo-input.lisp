(in-package #:client.core)

(def-view :photo-loader (props)
  ((:div class :photo-loader)
   (:label
    ((:input type "file"
	     name "file"
	     on-change (@ props on-change))))
   (:span "Загрузить фото")))

(def-widget :category-photo-input
  (add-file (file)
    (chain console (log file))
    (when file
      (upload-file
       file
       (http-handler (name)
	 (chain this props
		(on-change
		 (if (@ this props value)
		     (chain this props value
			    (concat (create name name
					    title (@ file name))))
		     (array (create name name
				    title (@ file name))))))))))
  (render ()
    ((:div class (:category-photo-input (@ this props class-name)))
     ((:label class (:activated
		     :label
		     (when (@ this props error)
		       :error)))
      (@ this props label))
     ((:div class :input)
      (when (@ this props value)
	(foreach ((file i) (@ this props value))
	  ((:div key i class :photo)
	   ((:a href (str "/image/" (@ file name))
		target "_blank")
	    ((:img src (str "/image/" (@ file name)))))
	   ((:text-button class :remove
			  on-click (event-handler ()
				     (chain this props
					    (on-change
					     (chain this props value
						    (filter (lambda (v)
							      (/= v file))))))))
	    "×"))))
      ((:photo-loader on-change (event-handler (ev)
				  (chain this (add-file (chain ev target files 0)))
				  (setf (@ ev target value) nil))))))))
