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

(defun get-register-32bit-name (name)
  "Translate a register's 64 name to its 32 bit name."
  (elt +register-names-32bit+ (position name +register-names+)))

(defun get-register-8bit-name (name)
  "Translate a register's 64 name to its 32 bit name."
  (elt +register-names-8bit+ (position name +register-names+)))

(defun operand-to-string (operand &optional (size :32))
  (check-type operand operand)
  (check-type size operand-size)
  (typecase operand
    (immediate (format nil "~d" (value operand)))
    (temp (format nil "~a ~(~a~)b" (name operand) size))
    (register (format nil "~(~a~)" (case size
                                     (:8 (get-register-8bit-name (name operand)))
                                     (:32 (get-register-32bit-name (name operand)))
                                     (:64 (name operand)))))
    (stack (format nil "~a PTR [rsp-~d]"
                   (case size
                     (:8 "BYTE")
                     (:32 "DWORD")
                     (:64 "QWORD"))
                   (offset operand)))))

(defmethod print-object ((obj label) out)
  (with-slots (name) obj
    (format out "~a:" name)))

(defmethod print-object ((obj call) out)
  (with-slots (name) obj
    (format out "call ~a" name)))

(defmethod print-object ((obj push-stack) out)
  (with-slots (arg1 size) obj
    (format out "push ~a" (operand-to-string arg1 size))))

(defmethod print-object ((obj pop-stack) out)
  (with-slots (arg1 size) obj
    (format out "pop ~a" (operand-to-string arg1 size))))

(defmethod print-object ((obj leave) out)
  (format out "leave"))

(defmethod print-object ((obj ret) out)
  (format out "ret"))

(defmethod print-object ((obj mov) out)
  (with-slots (source dest size) obj
    (format out "mov ~a, ~a"
            (operand-to-string dest size)
            (operand-to-string source size))))

(defmethod print-object ((obj cmp) out)
  (with-slots (arg1 arg2 size) obj
    (format out "cmp ~a, ~a"
            (operand-to-string arg2 size)
            (operand-to-string arg1 size))))

(defmethod print-object ((obj jmp) out)
  (with-slots (target) obj
    (format out "jmp ~a" target)))

(defmethod print-object ((obj jmpcc) out)
  (with-slots (target jump-cond) obj
    (format out "~a ~a"
            (case jump-cond
              (:zero "jz"))
            target)))

(defmethod print-object ((obj setcc) out)
  (with-slots (arg1 set-cond size) obj
    (format out "set~a ~a"
            (case set-cond
              (:equal "e")
              (:not-equal "ne")
              (:less "l")
              (:less-equal "le")
              (:greater "g")
              (:greater-less "ge"))
            (operand-to-string arg1 size))))

(defmethod print-object ((obj cdq) out)
  (format out "cdq"))

(defmethod print-object ((obj idiv) out)
  (with-slots (arg1 size) obj
    (format out "idiv ~a" (operand-to-string arg1 size))))

(defmethod print-object ((obj unary) out)
  (with-slots (opcode arg1 size) obj
    (format out "~(~a~) ~a" opcode (operand-to-string arg1 size))))

(defmethod print-object ((obj binary) out)
  (with-slots (opcode arg1 arg2 size) obj
    (format out "~(~a~) ~a, ~a" opcode
            (operand-to-string arg2 size)
            (operand-to-string arg1 size))))
