;;;; Code generation from IR

(in-package :codegen)

(defun codegen (iprogram)
  "Generate assembly code for IPROGRAM."
  (check-type iprogram ir:iprogram)
  (let ((asm-program (make-instance 'asm-program)))
    (loop for ifunc across (ir:functions iprogram) do
      (let ((asm-func (translate-ifunc ifunc)))
        (allocate-registers asm-func)
        (fixup-instructions asm-program asm-func)
        (format t "~a~%" asm-func)
        (vector-push-extend asm-func (functions asm-program))))
    asm-program))

(defun output-assembly (out asm-program)
  "Write ASM-PROGRAM as assembly code to output stream OUT."
  (check-type out stream)
  (check-type asm-program asm-program)
  (format out ".intel_syntax noprefix~%~%")
  (loop for func across (functions asm-program) do
    (format out ".globl ~a~%" (name func))
    (format out "~a:~%" (name func))
    (loop for instruction across (instructions func) do
      (if (typep instruction 'label)
          (format out "~a~%" instruction)
          (format out "~c~a~%" #\tab instruction)))
    (format out "~%"))
  (format out ".section .note.GNU-stack,\"\",@progbits~%"))

(defun translate-ifunc (ifunc)
  "Translate IFUNC into an `asm-func'."
  (check-type ifunc ir:ifunc)
  (let ((asm-func (make-instance 'asm-func :name (ir:name ifunc))))
    (resolve-allocs asm-func ifunc)
    (translate-initializers asm-func ifunc)
    (loop for statement across (ir:statements ifunc) do
      (translate-ir statement asm-func))
    asm-func))

(defun resolve-allocs (asm-func ifunc)
  "Map pointers returned from `alloc' in IFUNC to `stack' addresses in ASM-FUNC."
  (check-type asm-func asm-func)
  (check-type ifunc ir:ifunc)
  (with-slots (offsets stack-size) asm-func
    (loop for statement across (ir:allocs ifunc) do
      (when (typep statement 'ir:alloc)
        (setf (gethash (ir:name (ir:result statement)) offsets)
              (make-stack stack-size))
        (incf stack-size 4)))))

(defun translate-initializers (asm-func ifunc)
  "Translate any initializer statements to asm `instruction''s in IFUNC."
  (check-type asm-func asm-func)
  (check-type ifunc ir:ifunc)
  (with-slots (instructions offsets) asm-func
    (loop for statement across (ir:allocs ifunc) do
      (when (typep statement 'ir:mem-store)
        ;; If the statement is initializing space for an argument
        (when (typep (ir:value statement) 'ir:arg)
          (let* ((arg (ir:value statement))
                 (param-offset 24) ; parameters start 24 bytes bellow the current frame
                 (dest-addr (gethash (ir:name (ir:addr statement)) offsets)))
            (if (<= (ir:arg-num arg) 6)
                ;; first 7 args are in registers
                (let* ((arg-reg-name (elt +argument-registers+ (ir:arg-num arg)))
                       (arg-reg (make-register arg-reg-name)))
                  (emit-mov asm-func arg-reg dest-addr))
                ;; the rest are stored in the previous stack frame; move them into variables in the
                ;; current frame
                (let ((param (make-param param-offset)))
                  (emit-mov asm-func param (make-register :rd10))
                  (emit-mov asm-func (make-register :rd10) dest-addr)))))))))

(defun make-new-temp (asm-func)
  "Generate a new unique temporary of size SIZE."
  (check-type asm-func asm-func)
  (with-slots (next-temp) asm-func
    (let ((temp (make-instance 'temp :name (format nil "r.~a" next-temp))))
      (incf next-temp)
      temp)))

(defun emit-mov (asm-func source dest)
  "Helper function for emitting mov instructions since they are used so frequently."
  (check-type asm-func asm-func)
  (check-type source operand)
  (check-type dest operand)
  (with-slots (instructions) asm-func
    (vector-push-extend (make-instance 'mov :source source :dest dest) instructions)))

(defun emit-cmp (asm-func arg1 arg2)
  "Helper function for emitting cmp instructions since they are used so frequently."
  (check-type asm-func asm-func)
  (check-type arg1 operand)
  (check-type arg2 operand)
  (with-slots (instructions) asm-func
    ;; Test if the second operand if a constant and assign it a temporary if so. Cmp can't have a
    ;; constant as it second operand.
    (if (typep arg2 'immediate)
        (let ((temp (make-new-temp asm-func)))
          (emit-mov asm-func arg2 temp)
          (vector-push-extend (make-instance 'cmp :arg1 arg1 :arg2 temp) instructions))
        (vector-push-extend (make-instance 'cmp :arg1 arg1 :arg2 arg2) instructions))))

(defun value-type-to-operand-type (value-type)
  "Convert an IR value type to and oeprand size."
  (case value-type
    (:i32 :32)))

(defun value-to-operand (value)
  "Converts and IR `value' to an assembly `operand'."
  (check-type value ir:value)
  (typecase value
    (ir:constant (make-instance 'immediate :value (ir:value value)))
    (ir:var (make-instance 'temp :name (ir:name value)))))

(defgeneric translate-ir (statement asm-func)
  (:documentation "Translate IR statements into assembly instructions."))

(defmethod translate-ir ((statement ir:label) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (vector-push-extend (make-instance 'label :name (ir:name statement)) instructions)))

(defmethod translate-ir ((statement ir:call) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (loop for arg across (ir:args statement)
          for arg-num = 0 then (1+ arg-num)
          do
             (let* ((arg-value (value-to-operand arg))
                    (reg-name (elt +argument-registers+ arg-num ))
                    (arg-reg (make-register reg-name)))
               (emit-mov asm-func arg-value arg-reg)))
    (vector-push-extend (make-instance 'call :name (ir:name statement)
                                             :arg-count (length (ir:args statement)))
                        instructions)
    (emit-mov asm-func (make-register :rax)
              (value-to-operand (ir:result statement)))))

(defmethod translate-ir ((statement ir:ret) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (when (ir:return-value statement)
      (emit-mov asm-func (value-to-operand (ir:return-value statement))
                (make-register :rax)))
    (vector-push-extend (make-instance 'leave) instructions)
    (vector-push-extend (make-instance 'ret) instructions)))

(defmethod translate-ir ((statement ir:mem-load) (asm-func asm-func))
  (with-slots (instructions offsets) asm-func
    (let ((source (gethash (ir:name (ir:addr statement)) offsets))
          (dest (value-to-operand (ir:result statement))))
    (emit-mov asm-func source dest))))

(defmethod translate-ir ((statement ir:mem-store) (asm-func asm-func))
  (with-slots (instructions offsets) asm-func
    (let ((source (value-to-operand (ir:value statement)))
          (dest (gethash (ir:name (ir:addr statement)) offsets)))
      (emit-mov asm-func source dest))))

(defmethod translate-ir ((statement ir:jump) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (vector-push-extend (make-instance 'jmp :target (ir:target statement)) instructions)))

(defmethod translate-ir ((statement ir:jump-zero) (asm-func asm-func))
  (with-slots (instructions) asm-func
    ;; cmp can't have a constant as its 2nd arg, so always make sure it's in a register
    (let* ((cond-value (value-to-operand (ir:jump-cond statement))))
      (emit-cmp asm-func (make-immediate 0) cond-value))
    (vector-push-extend (make-instance 'jmpcc :target (ir:target statement)
                                              :jump-cond :zero)
                        instructions)))

(defun ir-unary-op-to-asm (opcode)
  (check-type opcode keyword)
  (case opcode
    (:neg :neg)))

(defmethod translate-ir ((statement ir:unary) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (let ((opcode (ir:opcode statement))
          (arg1 (value-to-operand (ir:arg1 statement)))
          (result (value-to-operand (ir:result statement))))
      (case opcode
        (:not
         (emit-cmp asm-func (make-immediate 0) arg1)
         (emit-mov asm-func (make-immediate 0) result)
         (vector-push-extend (make-instance 'setcc :set-cond :equal
                                                   :arg1 (make-temp (name result))
                                                   :size :8)
                             instructions))
        (otherwise
         (emit-mov asm-func arg1 result)
         (vector-push-extend (make-instance 'unary :opcode (ir-unary-op-to-asm opcode)
                                                   :arg1 result)
                             instructions))))))

(defun ir-binary-op-to-asm (opcode)
  (check-type opcode keyword)
  (case opcode
    (:add :add)
    (:sub :sub)
    (:mul :imul)))

(defmethod translate-ir ((statement ir:binary) (asm-func asm-func))
  (with-slots (instructions) asm-func
    (let ((opcode (ir:opcode statement))
          (arg1 (value-to-operand (ir:arg1 statement)))
          (arg2 (value-to-operand (ir:arg2 statement)))
          (result (value-to-operand (ir:result statement))))
      (case opcode
        (:div
         (let ((temp (make-new-temp asm-func)))
           (emit-mov asm-func arg2 temp)
           (emit-mov asm-func arg1 (make-register :rax))
           (vector-push-extend (make-instance 'cdq) instructions)
           (vector-push-extend (make-instance 'idiv :arg1 temp) instructions)
           (emit-mov asm-func (make-register :rax) result)))
        (otherwise
         (emit-mov asm-func arg1 result)
         (vector-push-extend (make-instance 'binary :opcode (ir-binary-op-to-asm opcode)
                                                    :arg1 arg2
                                                    :arg2 result)
                             instructions))))))
