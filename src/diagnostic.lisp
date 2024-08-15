;;;; Compiler diagnostic functionality

(in-package :diagnostic)

(defclass diagnostic ()
  ((level
    :initarg :level
    :reader level
    :documentation "Diagnostic type (e.g warning, error)")
   (line
    :initarg :line
    :reader line
    :documentation "Line the diagnostic occured on.")
   (message
    :initarg :message
    :reader message)))

(defmethod print-object ((obj diagnostic) out)
  (with-slots (level line message) obj
    (format out "line ~a: ~a ~a~%" line level message)))
