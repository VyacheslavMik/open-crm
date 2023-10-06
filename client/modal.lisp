(in-package #:client.core)

(defvar *modals* (create))

(eval-when (:compile-toplevel)
  (defpsmacro def-modal (name &body body)
    `(progn
       (setf (getprop *modals* ,name) ,name)
       (def-widget ,name ,@body))))

(defun modal-exists-p (modal)
  (getprop *modals* modal))

(def-modal :not-found-modal
  (render ()
    (:div "Modal not found")))

(defun show-modal (modal &optional props)
  (when *show-modal*
    (if (modal-exists-p modal)
	(funcall *show-modal* modal (or props (create)))
	(funcall *show-modal* :not-found-modal))))

(defun close-modal ()
  (when *close-modal*
    (funcall *close-modal*)))
