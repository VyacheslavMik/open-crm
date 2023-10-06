(in-package #:server.core)

(ps::define-expression-operator fun (name lambda-list &rest body)
  (multiple-value-bind (effective-args body-block docstring)
      (ps::compile-named-function-body name lambda-list body)
    (list 'fun% name effective-args docstring body-block)))

(ps::defprinter fun% (name args docstring body-block)
  (when docstring (ps::print-comment docstring))
  (ps::print-fun-def name args body-block))


(ps::define-expression-operator regexp (&rest forms)
  `(regexp% ,@(mapcar #'ps::ps-compile forms)))

(defun ps-print-string (s)
  (ps::psw ps::*js-string-delimiter*)
  (loop for char across s do
    (ps::acond
      ((char= char #\\)
       (ps::psw char char))
      
      ((getf ps::*js-lisp-escaped-chars* char)
       (ps::psw #\\ ps::it))

      ((or (<= (char-code char) #x1F)
	   (<= #x80 (char-code char) #x9F)
	   (member (char-code char) '(#xA0 #xAD #x200B #x200C)))
       (format ps::*psw-stream* "\\u~:@(~4,'0x~)" (char-code char)))

      (t
       (ps::psw char))))
  (ps::psw ps::*js-string-delimiter*))

(ps::defprinter regexp% (&rest forms)
  "new RegExp("
  (loop for (form . rest) on forms
	do (if (stringp form)
	       (ps-print-string form)
	       (ps::ps-print form))
	when rest
	  do (ps::psw "+"))
  ")")
