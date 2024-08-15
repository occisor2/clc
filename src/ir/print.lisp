;;;; Print methods for IR ast classes

(in-package :ir)

(defmethod print-object ((obj iprogram) out)
  (with-slots (functions) obj
    (format out "IProgram:~%")
    (map 'vector #'(lambda (ifunc)
                     (format out "~a~%" ifunc))
         functions)))

(defmethod print-object ((obj ifunc) out)
  (with-slots (name return-type params allocs statements) obj
    (format out "IFunc ~a (~{~a~^, ~}) -> ~(~a~)~%" name (coerce params 'list) return-type)
    (map 'vector #'(lambda (statement)
                     (format out "~c~a~%" #\tab statement))
         allocs)
    (map 'vector #'(lambda (statement)
                     (format out "~c~a~%" #\tab statement))
         statements)))

(defmethod print-object ((obj var) out)
  (with-slots (value-type name) obj
    (format out "~a ~(~a~)" name value-type)))

(defmethod print-object ((obj arg) out)
  (with-slots (value-type name) obj
    (format out "~a ~(~a~)" name value-type)))

(defmethod print-object ((obj constant) out)
  (with-slots (value-type value) obj
    (format out "~d ~(~a~)" value value-type)))

(defmethod print-object ((obj label) out)
  (with-slots (name) obj
    (format t "label ~a" name)))

(defmethod print-object ((obj call) out)
  (with-slots (name args result) obj
    (format t "~a = call ~a (~{~a~^, ~})" result name (coerce args 'list))))

(defmethod print-object ((obj ret) out)
  (with-slots (return-value) obj
    (format out "ret")
    (when return-value
      (format out " ~a" return-value))))

(defmethod print-object ((obj alloc) out)
  (with-slots (size result) obj
    (format out "~a = alloc ~(~a~)" result size)))

(defmethod print-object ((obj mem-load) out)
  (with-slots (addr result) obj
    (format out "~a = mem-load ~a" result addr)))

(defmethod print-object ((obj mem-store) out)
  (with-slots (value addr) obj
    (format out "mem-store ~a, ~a" value addr)))

(defmethod print-object ((obj jump) out)
  (with-slots (target) obj
    (format out "jump ~a" target)))

(defmethod print-object ((obj jump-zero) out)
  (with-slots (target jump-cond) obj
    (format out "jump-zero ~a, ~a" target jump-cond)))

(defmethod print-object ((obj unary) out)
  (with-slots (opcode arg1 result) obj
    (format out "~a = ~(~a~) ~a" result opcode arg1)))

(defmethod print-object ((obj binary) out)
  (with-slots (opcode arg1 arg2 result) obj
    (format out "~a = ~(~a~) ~a, ~a" result opcode arg1 arg2)))
