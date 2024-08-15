;;;; Print methods for assembly types

(in-package :codegen)

(defmethod print-object ((obj asm-program) out)
  (with-slots (functions) obj
    (format out "AsmProgram:~%")
    (map 'vector #'(lambda (func)
                     (format out "~a~%" func))
         functions)))

(defmethod print-object ((obj asm-func) out)
  (with-slots (name instructions stack-size saves) obj
    (format out "AsmFunc ~a:~%" name)
    (format out "~cLocals Space: ~a~%" #\tab stack-size)
    (format out "~cSaved registers: ~{~a~^, ~}~%" #\tab saves)
    (map 'vector #'(lambda (instruction)
                     (if (typep instruction 'label)
                         (format out "~a~%" instruction)
                         (format out "~c~a~%" #\tab instruction)))
         instructions)))

(defmethod print-object ((obj immediate) out)
  (with-slots (value) obj
    (format out "~d" value)))

(defmethod print-object ((obj temp) out)
  (with-slots (name size) obj
    (format out "~a ~(~a~)" name size)))

(defun get-register-32bit-name (name)
  "Translate a register's 64 name to its 32 bit name."
  (elt +register-names-32bit+ (position name +register-names+)))

(defmethod print-object ((obj register) out)
  (with-slots (name size) obj
    (format out "~(~a~)" (case size
                           (:64 name)
                           (:32 (get-register-32bit-name name))))))

(defmethod print-object ((obj stack) out)
  (with-slots (offset) obj
    (format out "DWORD PTR [rsp")
    (unless (zerop offset)
      (format out "-~d" offset))
    (format out "]")))

(defmethod print-object ((obj label) out)
  (with-slots (name) obj
    (format out "~a:" name)))

(defmethod print-object ((obj call) out)
  (with-slots (name) obj
    (format out "call ~a" name)))

(defmethod print-object ((obj push-stack) out)
  (with-slots (arg1) obj
    (format out "push ~a" arg1)))

(defmethod print-object ((obj pop-stack) out)
  (with-slots (arg1) obj
    (format out "pop ~a" arg1)))

(defmethod print-object ((obj leave) out)
  (format out "leave"))

(defmethod print-object ((obj ret) out)
  (format out "ret"))

(defmethod print-object ((obj mov) out)
  (with-slots (source dest) obj
    (format out "mov ~a, ~a" dest source)))

(defmethod print-object ((obj cmp) out)
  (with-slots (arg1 arg2) obj
    (format out "cmp ~a, ~a" arg2 arg1)))

(defmethod print-object ((obj jmp) out)
  (with-slots (target) obj
    (format out "jmp ~a" target)))

(defmethod print-object ((obj jmpcc) out)
  (with-slots (target jump-cond) obj
    (format out "~a ~a"
            (case jump-cond
              (:zero "jz"))
            target)))

(defmethod print-object ((obj unary) out)
  (with-slots (opcode arg1) obj
    (format out "~(~a~) ~a" opcode arg1)))

(defmethod print-object ((obj binary) out)
  (with-slots (opcode arg1 arg2) obj
    (format out "~(~a~) ~a, ~a" opcode arg2 arg1)))
