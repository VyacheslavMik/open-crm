(in-package #:server.core)

(defun write-json-chars (s stream)
  "Write JSON representations (chars or escape sequences) of
characters in string S to STREAM."
  (loop for ch across s
	for code = (char-code ch)
	with special
	if (setq special (car (rassoc ch json::+json-lisp-escaped-chars+)))
	  do (write-char #\\ stream) (write-char special stream)
	else
	  do (write-char ch stream)))

(setf (symbol-function 'json::write-json-chars) #'write-json-chars)
