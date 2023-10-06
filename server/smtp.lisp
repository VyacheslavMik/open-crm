(in-package #:server.core)

(defun write-blank-line (stream)
  (write-char #\Return stream)
  (write-char #\NewLine stream)
  (force-output stream))

(defun rfc2045-q-encode-string-fixed (str &key (external-format :utf-8))
  (let ((line-has-non-ascii nil)
        (last-line-break 0)
        (len (length str))
        (exformat (flex:make-external-format external-format)))
    (with-output-to-string (s)
      (loop for c across str
         for n from 0 to len
         for column = (- n last-line-break)
         do
           (when (>= column 74)
             (write-blank-line s)
             (write-char #\Space s)
             (setf last-line-break n))
           (cond
             ((char= c #\NewLine)
              (setf last-line-break n)
              (write-blank-line s)
              (write-char #\Space s))
             (t
              (unless line-has-non-ascii
                (format s "=?~A?Q?" 
                        (string-upcase (symbol-name external-format)))
                (setf line-has-non-ascii t))
              (loop for byte across (flex:string-to-octets 
                                     (make-string 1 :initial-element c)
                                     :external-format exformat)
                 do (format s "~:@(=~2,'0X~)" byte)))))
      (when line-has-non-ascii
        (format s "?=")))))

(setf (symbol-function 'cl-smtp::rfc2045-q-encode-string) #'rfc2045-q-encode-string-fixed)

(defun send-email (to body subject &optional display-name)
  (labels ((get-display-name ()
	     (or display-name
		 (let ((email-settings (query-email-settings)))
		   (get-v email-settings :name)))))
    (let ((server         (config/get :email :server))
	  (username       (config/get :email :username))
	  (password       (config/get :email :password))
	  (from           (config/get :email :from)))
      (cl-smtp:send-email server from to subject ""
			  :html-message body
			  :authentication (list username password)
			  :ssl :tls
			  :display-name (get-display-name)))))
