(in-package #:server.core)

(defparameter css
  (lass:compile-and-write
   '(html :height "100%"
          :width "100%"
          :display "flex"
     (body :margin 0
           :font-family "Roboto,sans-serif"
           :font-weight "400"
	   :background "#F8F9FA"
	   :height "100%"
	   :width "100%"

      (div :display "flex"
	   :font-size "18px")

      (input :font-family "Roboto,sans-serif")

      (.hidden :visibility "hidden")

      (div#root :height "100%"
		:width "100%"
		:margin 0)

      (.static-page
       :flex-direction "column"
       :margin "0 auto"
       :max-width "900px"

       (.header
	:margin "12px auto 20px auto"

	(.gap
	 :width "24px"))

       (.content
	:padding "12px 24px"
	:background "white"
	:border-radius "12px"
	:flex-direction "column"

	(code
	 :background "#eee"
	 :padding "2px 4px")

	((:and .form .feedback-form)
	 :flex-direction "column"
	 :margin-top "18px"

	 (.submit-button
	  :margin "24px auto 12px 0")

	 (.text-input
	  :margin-bottom "24px"
	  :width "50%")

	 (.textarea-input
	  :width "100%"

	  (textarea
	   :height "500px")))))

      (.app :width "100%"
	    :height "100%"
	    :position "relative"

       (.notifications
	:position "absolute"
	:width "100%"
	:height "100%"
	:background "rgba(0,0,0,0.4)"
	:z-index "100"
	(.notification
	 :border-radius "8px"
	 :background "white"
	 :max-height "100%"
	 :overflow-y "auto"

	 (.message
	  :padding "40px 80px"
	  :align-items "center"
	  (.error-icon
	   :width "20px"
	   :height "20px"
	   :margin-right "10px")
	  (span
	   :flex "1"))

	 (.button
	  :padding "14px 0"
	  :justify-content "center"
	  :border-top "1px solid gray"))))

      (.error-icon
       :stroke "red"
       :font-size "64px")

      (.centered
       :position "fixed"
       :top "50%"
       :left "50%"
       :transform "translate(-50%, -50%)")

      (.column :flex-direction "column")

      (button :all "unset")

      ;; Layout before outside the app
      (.out-page :width "100%"
		 :height "100%")

      (.out-form :width "100%"
		 :height "100%")

      (.out-container :width "100%"
		      :height "100%"

       (.out :margin "auto"

	(h2 :text-align "center")

	(.form-field :margin-bottom "20px")

	(.form-field
	 (.text-input :width "100%"))

	(.submit-button :justify-content "center")))
      ;; end

      ((:or .text-button .contained-button .outlined-button)
       :padding "10px 20px"
       :text-transform "uppercase"
       :font-weight "500"
       :white-space "nowrap"
       :letter-spacing "0.8px"
       :cursor "pointer"
       :text-align "center"
       :justify-content "center")
      ((:or .text-button .outlined-button) :color "rgb(72,35,208)")
      (.outlined-button :border-radius "8px"
			:border "1px solid rgb(72,35,208)"
                        :background "white")
      (.contained-button :background "rgb(72,35,208)"
			 :color "white"
			 :border-radius "8px")

      (.page :padding "20px"
	     :flex "1"
	     :overflow "scroll"

       (.header :align-items "center")

       ((:and .header .booking)
	(h3 :margin-right "20px")
	(.active			; :background "red"
	 (.text-button :color "black"
		       :cursor "default")))

       (.form :flex-direction "column"
	(.form-field :margin-bottom "20px")
	(.form-field
	 ((:or
	   .text-input
	   .textarea-input
	   .select-input
	   .date-input
	   .integer-input
	   .category-price-input
	   .category-photo-input) :width "100%")))

       ((:and .field .narrow) :width "150px"
	(.date-input :width "150px")))

      (.container :margin "0 auto 0 0"
                  :min-width "600px"
		  :flex 1)

      (.menu :border-right "1px solid rgba(0,0,0,.12)"
             :width "380px"
             :flex-direction "column"
	     :background "white"
	     :overflow-y "scroll")

      (.menu-item :padding "12px 48px 12px 48px"
                  :font-size "14px"
                  :line-height "18px"
                  :color "#5f6368"
                  :cursor "pointer")

      (.logout :margin-top "auto"
	       :margin-bottom "10px")

      ((:and .menu-item :hover) :background "rgba(0, 0, 0, 0.09)")

      ((:and .menu-item .selected) :color "#202124"
                                   :font-weight "600")

      (.menu-category :padding "12px 48px 12px 48px"
                      :margin-top "28px"
                      :font-size "18px"
                      :cursor "default"

       (span :margin-right "16px")

       (.icon :margin-left "auto"

	(img :width "16px")))

      ((:and .menu-category :hover) :background "rgba(0, 0, 0, 0.09)")

      ((:and .menu-category .selected) :color "#202124"
                                       :font-weight "600")

      (.photo-loader :position "relative"
                     :border-radius "3px"
                     :padding "10px 20px"
                     :justify-content "center"
                     :cursor "pointer"
                     :border "1px solid #ccc"
       (label :opacity 0
	      :width "100%"
	      :height "100%"
	      :position "absolute"
	      :top 0
	      :left 0
	      :cursor "pointer"
        (input :display "none")))

      ((:or
	.select-input
	.text-input
	.textarea-input
	.date-input
	.integer-input
	.sent-offer-field
	.sent-offer-transfer-list
	.sent-offer-service-list
	.category-price-input
	.category-photo-input) :flex-direction "column"
			       :position "relative"
       (.label :position "absolute"
	       :top "10px"
	       :left "15px"
	       :font-size "18px"
	       :font-weight "300"
	       :transition "top 0.3s, color 0.3s, font-size 0.3s"
	       :color "gray")
       ((:and .label (:or .activated .focused)) :top "-6px"
						:left "11px"
						:font-size "12px"
						:transition "top 0.3s, color 0.3s, font-size 0.3s"
						:background "white"
						:padding "0 4px")
       ((:and .label .focused) :color "#4823d0")
       ((:and .label .activated) :color "gray")
       ((:and .label (:or .focused .activated) .label-error) :color "red")
       ((:and div (:or .description .error-text)) :font-size "12px"
						  :margin-top "5px"
						  :margin-left "15px")
       ((:and div .description) :font-weight "300"
				:color "gray")
       ((:and div .error-text) :color "red")
       ((:or .input input textarea)
	:outline "none"
	:background "white"
	:font-size "18px"
	:padding "10px 15px"
	:border-radius "8px"
	:border "1px solid gray")
       ((:and (:or .input input textarea) .focused)
	:border "1px solid #4823d0")
       ((:and (:or .input input textarea .calendar) .input-error)
	:border "1px solid red"))
      
      ((:or
	(.select-input .items)
	(.date-input .calendar)) :flex-direction "column"
				 :position "absolute"
				 :top "100%"
				 :left "-1px"
				 :width "100%"
				 :z-index 1
				 :background "white"
				 :border-radius "0 0 8px 8px"
				 :border "1px solid #4823d0"

       (.item :padding "10px 20px"
	      :cursor "default")

       (.empty-value :color "gray"
		     :font-weight "300")

       ((:and .item :hover) :background "rgba(0, 0, 0, 0.05)")

       (.top-line :border-top "1px solid gray"))
      
      ((:or .date-input .select-input)
       (.input :height "21px"
	       :position "relative")
       ((:and .input .focused) :border-radius "8px 8px 0 0"
	((:or .calendar .items) :border-top-color "gray"))
       (.value :padding-right "10px"
	       :display "block"
	       :overflow "hidden"
	       :text-overflow "ellipsis"
	       :white-space "nowrap"
	       :cursor "default"))

      ;; date-input
      ((:and .date-input .focused) :z-index "1"
       ((:or .description .error-text) :z-index "-2"))

      (.date-input

       (input.value :border "none"
		    :border-radius "0"
		    :padding "0")

       (.calendar :width "300px"
		  :padding "15px"
		  :flex-direction "column"

        (.calendar-header

	 (.month-year :margin-left "auto"
		      :margin-right "auto")

	 (.month :cursor "default")

	 (.year :margin-left "10px"
		:cursor "default")

	 (.arrow-left :padding "10px"
		      :cursor "pointer"
		      :margin-left "8px"
	  (i :border "solid gray"
	     :margin-left "3px"
	     :border-width "0 2px 2px 0"
	     :display "inline-block"
	     :padding "3px"
	     :transform "rotate(135deg)"
	     :height "3px"
	     :width "2px"))

	 (.arrow-right :padding "10px"
		       :cursor "pointer"
		       :margin-right "8px"
	  (i :border "solid gray"
	     :margin-right "3px"
	     :border-width "0 2px 2px 0"
	     :display "inline-block"
	     :padding "3px"
	     :transform "rotate(315deg)"
	     :height "3px"
	     :width "2px")))
	(.days :text-align "center"
	 (.week-day :cursor "default"
		    :font-weight "600")
	 (.day :width "30px"
	       :height "30px")
	 ((:and .day .current) :border "1px solid #4823d0"
			       :border-radius "30%")
	 ((:and .day .selected) :background "rgba(72,35,208,0.1)")
	 ((:and .day .not-empty) :cursor "pointer"))
	((:and (:or .arrow-left .arrow-right (:and .day .not-empty)) :hover)
	 :background "rgba(0, 0, 0, 0.05)")))

      ((:and .input .focused .wide)   :border-radius "8px 8px 8px 0")
      ((:and .input .focused .narrow) :border-bottom "1px solid gray")
      ((:and .input .focused)
       ((:and .calendar .wide) :border-radius "0 8px 8px 8px"
			       :border-top-color "#4823d0"
			       :z-index "-1"))
      ((:and .input .focused .input-error)
       ((:and .calendar .wide) :border-top-color "red"))

      ;; select-input
      (.select-input
       (.down-arrow :margin-top "3px"
		    :margin-left "auto"
	(i :border "solid gray"
	   :border-width "0 2px 2px 0"
	   :display "inline-block"
	   :padding "3px"
	   :transform "rotate(45deg)"
	   :height "3px"
	   :width "2px"))
       ((:and .input .focused)
	(.down-arrow :margin-top "3px"
		     :transform "rotate(180deg)"
	 (i :border-color "#4823d0"))))

      (.category-price-input

       (.input

	(.new :margin-top "20px"
	      :margin-bottom "10px")

	((:and .date-input (:or .start .end)) :width "200px"
					      :margin-right "15px")

	((:and .integer-input .value) :width "150px")

	(.item :padding "0 0 0 15px"
	       :align-items "center"

	 (.remove :font-size "35px"
		  :padding-top "0"
		  :padding-bottom "0"
		  :margin-left "auto")))

       (.add-button :padding-bottom "19px"))

      (.category-photo-input
       (.input :max-width "739px"
	       :flex-wrap "wrap")
       (.photo-loader :align-items "center"
		      :justify-content "center"
		      :width "180px"
		      :height "180px"
		      :padding "10px"
		      :margin-top "10px")
       (.photo :border "1px solid lightgray"
	       :position "relative"
	       :margin-top "10px"
	       :margin-right "10px"
	       :padding "10px"
	       :width "180px"
	       :height "180px"
	       :justify-content "center"
	       :align-items "center"
	(.remove :position "absolute"
		 :top "-20px"
		 :right "-25px")
	(a :align-items "center"
	   :justify-content "center"
	   :width "180px"
	   :height "180px"
	   :display "flex"
	 (img :max-width "100%"
	      :max-height "100%"))))

      ((:or
	.offer-serivce-input
	.offer-transfer-input) :flex "1"

       (.add-button :padding-bottom "19px"))

      ((:and .form .category-form)
       (.submit-button :margin-right "10px")
       (.form-field :margin-right "10px"))

      ((:and .form .service-form)
       (.submit-button :margin-right "10px")
       (.form-field :margin-right "10px"
	(.price-type :width "200px")))

      ((:and .form .transfer-form)
       (.submit-button :margin-right "10px")
       (.type :width "200px")
       (.form-field :margin-right "10px"))

      ((:and .form .user-form)
       (.submit-button :margin-right "10px")
       (.form-field :width "200px"
		    :margin-right "10px"))

      ((:and .form .hotel-form)
       (.submit-button :margin-right "10px")
       (.form-field :width "700px"
		    :margin-right "10px")
       (textarea :width "700px"
		 :height "500px"))

      ((:and .form .email-settings-form)
       (.submit-button :margin-right "auto"))

      ((:and .form .template-form)
       (.submit-button :margin-right "auto")
       (textarea :width "700px"
		 :height "500px"))

      ((:and .form .email-settings-form)
       (.form-field :margin-right "10px"
		    :width "250px"))

      (.booking
       (.save :margin-left "10px")
       (.car-form :margin-bottom "24px")
       (.margin :padding "12px 0"))

      ((:and .form .offer-form)
       ((:or .close .send-by-email-button)
	:margin-right "10px")
       (.room-category :flex "1")
       ((:or
	 .room-price-period
	 .room-price
	 .room-total-price
	 .total-price) :margin-bottom "20px")
       ((:or
	 .offer-service
	 .offer-transfer) :margin-top "20px"
	((:or .date-time .comment) :margin-top "20px")
	(.date :width "200px"
	       :margin-right "10px")
	(.time :width "250px")
       	(.text-button :padding-top 0
       		      :padding-bottom 0
       		      :font-size "42px"
       		      :line-height 0
       		      :align-items "center"))
       (.offer-service :padding "10px 0"
		       :border "1px solid gray"
		       :border-radius "8px"
		       :background "white")
       (.form-field :margin-right "10px"
	((:or .check-in .check-out .phone .email) :width "220px")))
      
      ((:or .room-price-period
	    .sent-offer-service-list
	    .sent-offer-transfer-list)
       (table :border "1px solid gray"
	      :border-radius "8px"
	      :background "white"
	      :border-spacing 0
	(td :padding "10px 15px")
	(thead
	 (td :color "gray"
	     :border-bottom "1px solid gray"))))

      ((:and .form .task-form)
       (.submit-button :margin-right "10px")
       (.date :margin-right "10px")
       (.description
	(textarea :width "540px"
		  :height "200px")))

      (.entities-page
       ((:and .entity .task) :max-width "350px"
	(.remove :margin-left "unset")))

      ((:and .task-view .admin) :border-bottom "1px solid lightgray"
				:margin-bottom "10px")
      (.task-view :padding-bottom "10px"
		  :width "100%"
       (h3 :align-self "center")
       (.deadline-fail :color "red")
       (.recorded :font-size "13px"
		  :color "#a6a7a8"
		  :margin-bottom "10px"))

      (.entities-page
       (.entity :padding "10px 20px"
		:margin-bottom "10px"
		:border "1px solid lightgray"
		:border-radius "8px"
		:cursor "pointer"
		:align-items "center"
		:background "white"
	(.remove :margin-left "auto")
	(.select :margin-left "auto"))
       (.new :margin-bottom "20px")
       (.field :margin-right "20px")
       ((:and .entity :hover) :background "rgba(0, 0, 0, 0.09)"))

      (.add-booking-source :align-self "flex-start")

      (.sent-offer-page :min-width "600px"
       ((:or
	 .room-price-period
	 .sent-offer-service-list
	 .sent-offer-transfer-list)

	:margin-bottom "20px"
	:margin-right "10px")

       (.controls
	:margin-top "20px"
	:margin-right "auto"

	(.close-button
	 :margin-left "20px"))

       (.sent-offer-field :flex "1"
			  :margin-right "10px"
			  :margin-bottom "20px"))

      ((:or .booking-payment .booking-checkining)
       (.remains :margin-left "10px")
       (.send-by-email-button :margin-right "10px")
       (.inputs :margin-bottom "20px")
       (.field-input :width "255px")
       (.form-field :margin-right "10px"))

      (.calendar-page :flex "1"
		      :flex-direction "column"
		      :overflow "hidden"

       (.interval :padding "20px 30px"

	(.date :width "150px")

	(.separator :padding "0 15px"))

       (.calendar-container :overflow "scroll"

	(table :border 0
	       :border-collapse "collapse")

	(th :text-align "left")

	(td :padding "5px"
	    :border "1px solid #ebedf2"
	    :max-width "32px")

	(.calendar-date :text-align "center"
			:min-width "35px"
			:max-width "35px"
			:background "#f4f4f4")

	(.room :background "#f4f4f4"
	       :max-width "unset")

	(.room-category :background "#fef0bf"

	 (td :padding "10px 20px"))

	(.category-cell :border "none")

	((:and .frozen .category-cell) :white-space "nowrap")

	(.category-booking :cursor "pointer"
			   :background "lightblue"
			   :text-overflow "ellipsis"
			   :overflow "hidden"
			   :white-space "nowrap")

	((:and .day-cell .drag-over) :background "lightgray")

	(.day-cell :background "white")

	(th :padding "10px 0"
	    :background "white")

	(.frozen :position "sticky"
		 :left 0)))

      (.upcoming-transfer :flex-direction "column"
       (span :margin-bottom "5px"))))))
