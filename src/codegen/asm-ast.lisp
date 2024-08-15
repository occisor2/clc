;;;; Assembly instruction and type hierarchy

(in-package :codegen)

(eval-when (:compile-toplevel)
  (defparameter +register-names+
    '(:rbp :rsp :rax :rbx :rcx :rdx :rdi :rsi :r8 :r9 :r10 :r11 :r12 :r13 :r14 :r15)))

(defparameter +register-names-32bit+
    '(:ebp :esp :eax :ebx :ecx :edx :edi :esi :r8d :r9d :r10d :r11d :r12d :r13d :r14d :r15d))

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

(deftype operand-size () '(member :32 :64))

(declaim (optimize safety))
(defclass operand ()
  ((size
    :initarg :size
    :accessor size
    :type operand-size)))

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

(deftype register-type () `(member ,@+register-names+))

(declaim (optimize safety))
(defclass register (operand)
  ((name
    :initarg :name
    :accessor name
    :type register-type)))

(defun make-register (name size)
  (check-type name keyword)
  (check-type size operand-size)
  (make-instance 'register :name name :size size))

(declaim (optimize safety))
(defclass stack (operand)
  ((offset
    :initarg :offset
    :accessor offset
    :type integer)))

(defun make-stack (offset size)
  (check-type offset integer)
  (check-type size operand-size)
  (make-instance 'stack :offset offset :size size))

;;;; Instruction types

(defclass instruction ()
  ((lives
    :initform nil
    :accessor lives
    :type list
    :documentation "List of live registers at this instruction.")))

(declaim (optimize safety))
(defclass label (instruction)
  ((name
    :initarg :name
    :accessor name
    :type string)))

(declaim (optimize safety))
(defclass call (instruction)
  ((name
    :initarg :name
    :accessor name
    :type string)
   (arg-count
    :initarg :arg-count
    :accessor arg-count
    :type integer
    :documentation "Only needed for the register allocation pass.")))

(declaim (optimize safety))
(defclass push-stack (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(declaim (optimize safety))
(defclass pop-stack (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

(declaim (optimize safety))
(defclass leave (instruction) ())

(declaim (optimize safety))
(defclass ret (instruction) ())

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

(declaim (optimize safety))
(defclass jmp (instruction)
  ((target
    :initarg :target
    :accessor target
    :type string
    :documentation "Name of the label this jump is targeting.")))

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

(declaim (optimize safety))
(defclass cdq (instruction) ())

(declaim (optimize safety))
(defclass idiv (instruction)
  ((arg1
    :initarg :arg1
    :accessor arg1
    :type operand)))

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

(deftype unary-opcode () '(member :add :sub :imul))

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
