(in-package #:asdf-user)

(defsystem #:open-crm
  :description "Customer-relationship management for hotels"
  :version "0.0.1"
  :author "Vyacheslav Mikushev <vyacheslav.mik.8@gmail.com>"
  :licence "Public Domain"
  :depends-on (#:hunchentoot
	       #:parenscript
	       #:cl-who
	       #:postmodern
	       #:cl-json
	       #:uuid
	       #:ironclad
	       #:cl-ppcre
	       #:lass
	       #:cl-smtp
	       #:clip
	       #:flexi-streams
	       #:drakma
	       #:cl-mustache
	       #:str)
  :components ((:file "packages")
	       (:file "style/core")
	       (:file "server/config")
	       (:file "server/smtp")
	       (:file "server/json")
	       (:file "server/routing")
	       (:file "server/utils")
	       (:file "server/db")
	       (:file "server/entity")
	       (:file "server/parenscript")
	       (:file "server/telegram")
               (:file "server/core")))
