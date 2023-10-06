(in-package #:client.core)

(def-shared bookings)

(defun init-shared ()
  (shared/set bookings #()))
