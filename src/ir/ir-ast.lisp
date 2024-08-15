;;;; IR type and statement hierarchy

(in-package :ir)

(defparameter *value-types*
  '(:ptr
    :i32)
  "Valid IR value types for reference.")

(defparameter *opcodes*
  '(:label
    :call
    :ret
    :alloc
    :mem-load
    :mem-store
    :jump
    :jump-zero
    ;; unary
    :neg
    ;; binary
    :add
    :sub
    :mul)
  "List of instructions for reference.")

(declaim (optimize safety))
(defclass iprogram ()
  ((functions
    :initarg :functions
    :accessor functions
    :type (array ifunc))))

(declaim (optimize safety))
(defclass ifunc ()
  ((name
    :initarg :name
    :accessor name
    :type string)
   (return-type
    :initarg :return-type
    :accessor return-type
    :type :keyword)
   (params
    :initarg :params
    :accessor params
    :type (array arg)
    :documentation "Array of function parameters.")
   (allocs
    :accessor allocs
    :type (array statement)
    :documentation "Array of alloc instructions for local variables and parameters.")
   (statements
    :accessor statements
    :type (array statement)
    :documentation "Array of statements making up the function's code.")
   (next-temp
    :initform 0
    :type integer
    :documentation "The next available number for a variable name.")
   (next-label
    :initform 0
    :type integer
    :documentation "The next available number for a label.")))

(defmethod initialize-instance :after ((obj ifunc) &key)
  (with-slots (params allocs statements) obj
    (setf params (make-array 0 :adjustable t :fill-pointer 0)
          allocs (make-array 0 :adjustable t :fill-pointer 0)
          statements (make-array 0 :adjustable t :fill-pointer 0))))

;;;; Basic value types

(declaim (optimize safety))
(defclass value ()
  ((value-type
    :initarg :value-type
    :accessor value-type
    :type keyword)))

(declaim (optimize safety))
(defclass var (value)
  ((name
    :initarg :name
    :accessor name
    :type string)))

(declaim (optimize safety))
(defclass arg (value)
  ((name
    :initarg :name
    :accessor name
    :type string)
   (arg-num
    :initarg :arg-num
    :accessor arg-num
    :type integer
    :documentation "Position of the argument in the parameter list.")))

(declaim (optimize safety))
(defclass constant (value)
  ((value
    :initarg :value
    :accessor value
    :type integer)))

;;;; Statement types

(declaim (optimize safety))
(defclass statement () ())

(declaim (optimize safety))
(defclass label (statement)
  ((name
    :initarg :name
    :accessor name
    :type string)))

(declaim (optimize safety))
(defclass call (statement)
  ((name
    :initarg :name
    :accessor name
    :type string)
   (args
    :initarg :args
    :accessor args
    :type (array value))
   (result
    :initarg :result
    :accessor result
    :type var)))

(declaim (optimize safety))
(defclass ret (statement)
  ((return-value
    :initarg :return-value
    :accessor return-value
    :type (or value null))))

(declaim (optimize safety))
(defclass alloc (statement)
  ((size
    :initarg :size
    :accessor size
    :type integer)
   (result
    :initarg :result
    :accessor result
    :type var)))

(declaim (optimize safety))
(defclass mem-load (statement)
  ((addr
    :initarg :addr
    :accessor addr
    :type var)
   (result
    :initarg :result
    :accessor result
    :type var)))

(declaim (optimize safety))
(defclass mem-store (statement)
  ((value
    :initarg :value
    :accessor value
    :type value)
   (addr
    :initarg :addr
    :accessor addr
    :type var)))

(declaim (optimize safety))
(defclass jump (statement)
  ((target
    :initarg :target
    :accessor target
    :type string
    :documentation "Name of the label this jump is targeting.")))

(declaim (optimize safety))
(defclass jump-zero (statement)
  ((target
    :initarg :target
    :accessor target
    :type string
    :documentation "Name of the label this jump is targeting.")
   (jump-cond
    :initarg :jump-cond
    :accessor jump-cond
    :type value)))

(declaim (optimize safety))
(defclass unary (statement)
  ((opcode
    :initarg :opcode
    :accessor opcode
    :type keyword)
   (arg1
    :initarg :arg1
    :accessor arg1
    :type value)
   (result
    :initarg :result
    :accessor result
    :type var)))

(declaim (optimize safety))
(defclass binary (statement)
  ((opcode
    :initarg :opcode
    :accessor opcode
    :type keyword)
   (arg1
    :initarg :arg1
    :accessor arg1
    :type value)
   (arg2
    :initarg :arg2
    :accessor arg2
    :type value)
   (result
    :initarg :result
    :accessor result
    :type var)))
