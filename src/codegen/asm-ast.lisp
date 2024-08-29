;;;; Assembly instruction and type hierarchy

(in-package :codegen)

(eval-when (:compile-toplevel)
  (defparameter +register-names+
    '(:rbp :rsp :rax :rbx :rcx :rdx :rdi :rsi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)))

(defparameter +register-names-32bit+
  '(:ebp :esp :eax :ebx :ecx :edx :edi :esi :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d))

(defparameter +register-names-8bit+
  '(:bpl :spl :al :bl :cl :dl :dil :sil :r8d :r9b :r10b :r11b :r12b :r13b :r14b :r15b))

(defparameter +argument-registers+
  '(:rdi :rsi :rdx :rcx :r8 :r9))

(defparameter +callee-saved-registers+
  '(:rbx :r12 :r13 :r14 :r15))

(defparameter +general-registers+
  '(:r9 :r8 :rsi :rdi :rdx :rcx :rax :rbx :r12 :r13 :r14 :r15))

(defparameter +opcodes+
  '(:label
    :call
    :push-stack
    :pop-stack
    :leave
    :ret
    :mov
    :cmp
    :jmp
    :jmpcc
    :setcc
    :cdq
    :idiv
    :neg
    :add
    :sub
    :imul)
  "Assembly instructions for reference.")

(declaim (optimize safety))
(defclass asm-program ()
  ((functions
    :initarg :functions
    :accessor functions
    :type (array asm-func))
   (label-count
    :initform 0
    :accessor label-count
    :type integer)
   (label-map
    :accessor label-map
    :type hash-table)))

(defmethod initialize-instance :after ((obj asm-program) &key)
  (with-slots (functions label-map) obj
    (setf functions (make-array 1 :adjustable t :fill-pointer 0)
          label-map (make-hash-table :test 'equal))))

(declaim (optimize safety))
(defclass asm-func ()
  ((name
    :initarg :name
    :accessor name
    :type string)
   (instructions
    :accessor instructions
    :type (array instruction))
   (offsets
    :accessor offsets
    :type hash-table
    :documentation "Lookup table for mapping pointers to stack offsets.")
   (stack-size
    :initform 0
    :accessor stack-size
    :type integer)
   (saves
    :initform nil
    :accessor saves
    :type list
    :documentation "List of registers that need to saved and restored from the register allocator.")
   (next-temp
    :initform 0
    :type integer
    :documentation "the next available number for a temporary register.")))

(defmethod initialize-instance :after ((obj asm-func) &key)
  (with-slots (instructions offsets) obj
    (setf instructions (make-array 0 :adjustable t :fill-pointer 0)
          offsets (make-hash-table :test 'equal))))

;;;; Operand types

(deftype operand-size () '(member :8 :32 :64))

(declaim (optimize safety))
(defclass operand () ())

(declaim (optimize safety))
(defclass immediate (operand)
  ((value
    :initarg :value
    :accessor value
    :type integer)))

(defun make-immediate (value)
  (check-type value integer)
  (make-instance 'immediate :value value))

(declaim (optimize safety))
(defclass temp (operand)
  ((name
    :initarg :name
    :accessor name
    :type string)))

(defun make-temp (name)
  (check-type name string)
  (make-instance 'temp :name name))

(deftype register-type () `(member ,@+register-names+))

(declaim (optimize safety))
(defclass register (operand)
  ((name
    :initarg :name
    :accessor name
    :type register-type)))

(defun make-register (name)
  (check-type name keyword)
  (make-instance 'register :name name))

(declaim (optimize safety))
(defclass stack (operand)
  ((offset
    :initarg :offset
    :accessor offset
    :type integer)))

(defun make-stack (offset)
  (check-type offset integer)
  (make-instance 'stack :offset offset))

(declaim (optimize safety))
(defclass param (operand)
  ((offset
    :initarg :offset
    :accessor offset
    :type integer)))

(defun make-param (offset)
  (check-type offset integer)
  (make-instance 'param :offset offset))

;;;; Instruction types

(deftype operand-size () '(member :8 :32 :64))

(defclass instruction ()
  ((size
    :initarg :size
    :accessor size
    :type operand-size)
   (lives
    :initform nil
    :accessor lives
    :type list
    :documentation "List of live registers at this instruction."))
  (:default-initargs :size :32))

(declaim (optimize safety))
(defclass label (instruction)
  ((name
    :initarg :name
    :accessor name
    :type string)))

(defun make-label (name)
  (make-instance 'label :name name))

(declaim (optimize safety))
(defclass call (instruction)
  ((name
    :initarg :name
    :accessor name
    :type string)
   (prototype
    :initarg :prototype
    :accessor prototype
    :type (array operand-size))
   (arg-count
    :initarg :arg-count
    :accessor arg-count
    :type integer
    :documentation "Only needed for the register allocation pass.")))

(defun make-call (name prototype)
  (make-instance 'call :name name :prototype prototype))

(declaim (optimize safety))
(defclass push-stack (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(defun make-push-stack (arg1)
  (make-instance 'push-stack :arg1 arg1 :operand-size :64))

(declaim (optimize safety))
(defclass pop-stack (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(defun make-pop-stack (arg1)
  (make-instance 'pop-stack :arg1 arg1 :operand-size :64))

(declaim (optimize safety))
(defclass leave (instruction) ())

(defun make-leave ()
  (make-instance 'leave))

(declaim (optimize safety))
(defclass ret (instruction) ())

(defun make-ret ()
  (make-instance 'ret))

(declaim (optimize safety))
(defclass mov (instruction)
  ((source
    :initarg :source
    :accessor source
    :type operand)
   (dest
    :initarg :dest
    :accessor dest
    :type operand)))

(defun make-mov (source dest operand-size)
  (make-instance 'mov :source source :dest dest :operand-size operand-size))

(declaim (optimize safety))
(defclass cmp (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)
   (arg2
    :initarg :arg2
    :accessor arg2
    :type operand)))

(defun make-cmp (arg1 arg2 operand-size)
  (check-type arg2 (not immediate))
  (make-instance 'cmp :arg1 arg1 :arg2 arg2 :operand-size operand-size))

(declaim (optimize safety))
(defclass jmp (instruction)
  ((target
    :initarg :target
    :accessor target
    :type string
    :documentation "Name of the label this jump is targeting.")))

(defun make-jmp (target)
  (make-instance 'jmp :target target))

(deftype jump-cond () '(member :zero))

(declaim (optimize safety))
(defclass jmpcc (instruction)
  ((target
    :initarg :target
    :accessor target
    :type string
    :documentation "Name of the label this jump is targeting.")
   (jump-cond
    :initarg :jump-cond
    :accessor jump-cond
    :type jump-cond)))

(defun make-jmpcc (target jump-cond)
  (make-instance 'jmpcc :target target :jump-cond jump-cond))

(deftype set-cond ()
  '(member :equal :not-equal :less :less-equal :greater :greater-equal))

(declaim (optimize safety))
(defclass setcc (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)
   (set-cond
    :initarg :set-cond
    :accessor set-cond
    :type set-cond)))

(defun make-setcc (arg1 set-cond)
  (make-instance 'setcc :arg1 arg1 :set-cond set-cond :operand-size :8))

(declaim (optimize safety))
(defclass cdq (instruction) ())

(defun make-cdq ()
  (make-instance 'cdq))

(declaim (optimize safety))
(defclass idiv (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(defun make-idiv (arg1 operand-size)
  (make-instance 'idiv :arg1 arg1 :operand-size operand-size))

(deftype unary-opcode () '(member :neg))

(declaim (optimize safety))
(defclass unary (instruction)
  ((opcode
    :initarg :opcode
    :accessor opcode
    :type unary-opcode)
   (arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(defun make-unary (arg1 operand-size)
  (make-instance 'unary :arg1 arg1 :operand-size operand-size))

(deftype binary-opcode () '(member :add :sub :imul))

(declaim (optimize safety))
(defclass binary (instruction)
  ((opcode
    :initarg :opcode
    :accessor opcode
    :type binary-opcode)
   (arg1
    :initarg :arg1
    :accessor arg1
    :type operand)
   (arg2
    :initarg :arg2
    :accessor arg2
    :type operand)))

(defun make-binary (arg1 arg2 operand-size)
  (make-instance 'binary :arg1 arg1 :arg2 arg2 :operand-size operand-size))
