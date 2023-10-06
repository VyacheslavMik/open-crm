(in-package #:server.utils)

(defun ttrav (rec &optional (base #'identity))
  (labels ((self (tree)
	     (if (atom tree)
		 (if (functionp base)
		     (funcall base tree)
		     base)
		 (funcall rec (self (car tree))
			  (if (cdr tree)
			      (self (cdr tree)))))))
    #'self))

(defun code-replace-symbol (code fn)
  (let (replacedp)
    (values
     (funcall (ttrav #'cons
		     (lambda (a)
		       (let ((replacement (funcall fn a)))
			 (if replacement
			     (progn
			       (setf replacedp t)
			       replacement)
			     a))))
	      code)
     replacedp)))

(defun substring (s start end)
  (if (> end (length s))
      (subseq s start (length s))
      (subseq s start end)))

(defun replace-symbol-this (s)
  (when (and (symbolp s) (eq s 'this)) (make-symbol "_THIS")))

(defparameter html-tags '(:a :abbr :acronym :address :applet :area :article :aside :audio :b :base :basefont
			  :bdi :bdo :big :blockquote :body :br :button :canvas :caption :center :cite :code
			  :col :colgroup :datalist :dd :del :details :dfn :dialog :dir :div :dl :dt :em :embed
			  :fieldset :figcaption :figure :font :footer :form :frame :frameset :h1 :h2 :h3 :h4
			  :h5 :h6 :head :header :hr :html :i :iframe :img :input :ins :kbd :keygen :label
			  :legend :li :link :line :main :map :mark :menu :menuitem :meta :meter :nav :noframes
			  :noscript :object :ol :optgroup :option :output :p :param :picture :pre :progress
			  :q :rp :rt :ruby :s :samp :script :section :select :small :source :span :strike
			  :strong :style :sub :summary :sup :svg :table :tbody :td :textarea :tfoot :th :thead
			  :time :title :tr :track :tt :u :ul :var :video :wbr))

(defun html-tag-p (k)
  (when (keywordp k)
    (find k html-tags)))

(defun starts-with-p (str1 str2)
  "Determine whether `str1` starts with `str2`"
  (let ((p (search str2 str1)))
    (and p (= 0 p))))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun make-uuid ()
  (with-output-to-string (s)
    (format s "~(~s~)" (uuid:make-v1-uuid))))

(defun hash-password (password)
  (when password
    (ironclad:pbkdf2-hash-password-to-combined-string
     (flexi-streams:string-to-octets password :external-format :utf-8))))

(defun check-password (password combined-salt-and-digest)
  (when password
    (ironclad:pbkdf2-check-password
     (flexi-streams:string-to-octets password :external-format :utf-8)
     combined-salt-and-digest)))

(defun get-v (json &rest path)
  (loop
     for k in path
     for test = (etypecase k
		  (string #'string=)
		  (keyword #'eql))
     for v = (cdr (assoc k json :test test)) then (cdr (assoc k v :test test))
     while v
     finally (return v)))

(defun (setf get-v) (new json &rest path)
  (loop
    for k in path
    for test = (etypecase k
		 (string #'string=)
		 (keyword #'eql))
    for v = (let ((v (assoc k json :test test)))
	      (unless v
		(rplacd json (cons (car json) (cdr json)))
		(rplaca json (cons k nil)))
	      (assoc k json :test test))
      then (let ((v* (assoc k (cdr v) :test test)))
	     (unless v*
	       (push (cons k nil) (cdr v)))
	     (assoc k (cdr v) :test test))
    finally (progn
	      (setf (cdr v) new)
	      (return json))))

(defun rem-v (json &rest path)
  (if (= (length path) 1)
      (let ((k (car path)))
	(remove k json :key #'car
		       :test (etypecase k
			       (string #'string=)
			       (keyword #'eql))))
      (error "Not implmeneted")))

(defun bad-request (reason)
  (setf (hunchentoot:return-code*) hunchentoot:+http-bad-request+)
  reason)

(defun invalid-password-p (password)
  (or (not (stringp password))
      (< (length password) 8)))

(defun invalid-email-p (email)
  (let ((email-regexp "^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}$"))
    (not (cl-ppcre:all-matches email-regexp email))))

(defun server-url ()
  (or (hunchentoot:header-in* "origin")
      (format nil "~a://~a"
	      (if (eq (hunchentoot:server-protocol*) :https)
		  "https"
		  "http")
	      (hunchentoot:host))))
