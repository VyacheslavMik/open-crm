(in-package #:cl-user)

(defpackage #:server.utils
  (:use #:cl)
  (:export #:code-replace-symbol
	   #:replace-symbol-this
	   #:html-tag-p
	   #:make-keyword
	   #:starts-with-p
	   #:substring
	   #:make-uuid
	   #:hash-password
	   #:check-password
	   #:get-v
	   #:bad-request
	   #:invalid-password-p
	   #:invalid-email-p
	   #:rem-v
	   #:server-url))

(defpackage #:server.core
  (:use #:cl #:server.utils)
  (:export #:fun
	   #:regexp))

#+()(progn
      (delete-package (find-package "CLIENT.CORE"))
      (delete-package (find-package "DATE"))
      (delete-package (find-package "CLIENT.UTILS")))

(defpackage #:client.utils
  (:use #:cl #:parenscript #:server.core)
  (:export #:str
	   #:query
	   #:foreach
	   #:clone-object
	   #:event-handler
	   #:http-handler
	   #:pred
	   #:set-cookie
	   #:get-cookie
	   #:printv
	   #:number-p
	   #:clicked-inside-p
	   #:emptyp
	   #:blank-stringp
	   #:conj
	   #:object-keys
	   #:find
	   #:join
	   #:merge-objects))

(defpackage #:client.core
  (:use #:cl
	#:parenscript
	#:client.utils
	#:server.core
	#:server.utils))

(defpackage #:date
  (:use #:cl
	#:parenscript
	#:client.utils)
  (:export #:month
	   #:year
	   #:date
	   #:now
	   #:make-ymd
	   #:parse-iso
	   #:unparse-iso
	   #:days-in-month
	   #:days-in-year
	   #:start-of-the-year
	   #:incf-month
	   #:incf-date
	   #:iso-date
	   #:iso-dm->date
	   #:iso-fd->date
	   #:ru-dm->date
	   #:unparse
	   #:next-month-start
	   #:month-start
	   #:days-between
	   #:parse-local-date
	   #:try-parse
	   #:d=
	   #:d/=
	   #:d>
	   #:d>=
	   #:d<
	   #:d<=
	   #:now-date))
