(in-package #:client.core)

(def-view :error-icon (props)
  ((:svg class :error-icon
	 view-box "0 0 12 12"
	 version "1.1"
	 xmlns "http://www.w3.org/2000/svg")
   ((:line x1 "1" y1 "11"
	   x2 "11" y2 "1"
	   stroke-width "2"))
   ((:line x1 "1" y1 "1"
	   x2 "11" y2 "11"
	   stroke-width "2"))))

(def-view :up-arrow-icon (props)
  ((:img src "static/image/up-arrow-icon.svg")))

(def-view :down-arrow-icon (props)
  ((:img src "static/image/down-arrow-icon.svg")))
