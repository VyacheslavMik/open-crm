(in-package #:client.utils)

(defun empty-object-p (obj)
  (and (= (chain -object (keys obj) length) 0)
       (= (@ obj constructor) -object)))

;; think about using this one
;; https://medium.com/@dtinth/immutable-js-persistent-data-structures-and-structural-sharing-6d163fbd73d2
(eval-when (:compile-toplevel)
  (defpsmacro clone-object (obj)
    `(chain -j-s-o-n (parse (chain -j-s-o-n (stringify ,obj)))))

  (defpsmacro id-generator ()
    (let ((cur (gensym)))
      `(let ((,cur 0))
	 (defun next-id ()
	   (prog1 ,cur (incf ,cur))))))

  (defpsmacro zerop (n)
    `(= ,n 0))
  
  (defpsmacro printv (&rest values)
    `(chain console (log ,@values))))

(id-generator)

(defun date-diff-in-days (date1 date2)
  (/ (- date1 date2) 86400000))

(defun as-hash-map (arr)
  (let ((hs (create)))
    (loop for item in arr
	  do (setf (getprop hs (@ item id)) item))
    hs))

(defun for-select-input (arr)
  (chain arr (map (lambda (item)
		    (create key   (@ item id)
			    label (@ item name))))))

(defun set-cookie (key value)
  (setf (@ document cookie) (+ key "=" (chain -j-s-o-n (stringify value)))))

(defun get-cookie (key)
  (let ((value (chain document cookie
		      (replace
		       (regexp "(?:(?:^|.*;\\s*)" key "\\s*\\=\\s*([^;]*).*$)|^.*$") "$1"))))
    (when value (chain -j-s-o-n (parse value)))))

(defun number-p (v)
  (let ((type (typeof v)))
    (or (= type "number")
	(and (= type "string")
	     (chain (regex "^-?\\d*$") (test v))))))

(defun stringp (v)
  (let ((type (typeof v)))
    (= type "string")))

(defun clicked-inside-p (ref event)
  (when (and ref (@ ref current))
    (chain ref current (contains (@ event target)))))

(defun emptyp (arr)
  (if arr
      (zerop (@ arr length))
      t))

(defun blank-stringp (s)
  (when (or (stringp s) (null s))
    (or (null s)
	(zerop (@ s length))
	(loop for ch in s
	      unless (= ch #\ )
		return nil
	      finally (return t)))))

(defun conj (arr v)
  (chain arr (push v))
  arr)

(defun object-keys (o)
  (if o
      (chain -object (keys o))
      #()))

(defun local-storage/store (key value)
  (chain window local-storage (set-item key value)))

(defun local-storage/restore (key)
  (chain window local-storage (get-item key)))

(defun filter (pred arr)
  (chain arr (filter pred)))

(defun find (pred arr)
  (chain arr (find pred)))

(defun join (arr sep)
  (chain arr (join sep)))

(defun merge-objects (o1 o2)
  (chain -object (assign o1 o2)))
