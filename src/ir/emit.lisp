;;;; IR emiting functions

(in-package :ir)

(defun make-ifunc (name return-type)
  "Create an IR function instance."
  (check-type name string)
  (check-type return-type keyword)
  (make-instance 'ifunc :name name :return-type return-type))

(defun make-constant (const const-type)
  "Create a new IR `constant' object."
  (check-type const integer)
  (make-instance 'constant :value const :value-type const-type))

(defun new-temp-number (ifunc)
  "Generate a new unique number for labeling variables."
  (check-type ifunc ifunc)
  (with-slots (next-temp) ifunc
    (let ((temp-num next-temp))
      (incf next-temp)
      temp-num)))

(defun new-temp-var (ifunc var-type)
  "Generate a new unique variable of type VAR-TYPE."
  (check-type ifunc ifunc)
  (check-type var-type keyword)
  (make-instance 'var :name (format nil "t.~a" (new-temp-number ifunc))
                      :value-type var-type))

(defun make-label-name (ifunc &optional (useful-name ""))
  "Generate a new unique label name. Optionaly a useful name can supplied. It will be mangled to
avoid collisions. "
  (check-type ifunc ifunc)
  (check-type useful-name string)
  (with-slots (next-label) ifunc
    (let ((name (format nil "~a.~a" useful-name next-label)))
      (incf next-label)
      name)))


(defun add-parameter (ifunc name param-type)
  "Add a new parameter to the function. Returns a pointer to memory where the parameter is stored."
  (check-type ifunc ifunc)
  (check-type name string)
  (check-type param-type keyword)
  (with-slots (params allocs) ifunc
    (let* ((new-param-name (format nil "~a.~a" name (new-temp-number ifunc)))
           (arg-number (length params))
           (new-arg (make-instance 'arg :name new-param-name
                                        :value-type param-type
                                        :arg-num arg-number))
           (alloc-ptr (emit-alloc ifunc param-type)))
      ;; add new argument to the parameter list
      (vector-push-extend new-arg params)
      ;; initialize the allocated memory with argument value
      (vector-push-extend (make-instance 'mem-store :value new-arg :addr alloc-ptr) allocs)
      ;; return pointer to allocated space
      alloc-ptr)))

(defun emit-label (ifunc name)
  "Emit a new ret statement with an optional return-value."
  (check-type ifunc ifunc)
  (check-type name string)
  (with-slots (statements) ifunc
    (vector-push-extend (make-instance 'label :name name) statements)))

(defun emit-call (ifunc name return-type args)
  "Emit a new ret statement with an optional return-value."
  (check-type ifunc ifunc)
  (check-type name string)
  (check-type args array)
  (with-slots (statements) ifunc
    (let ((result (new-temp-var ifunc return-type)))
      (vector-push-extend (make-instance 'call :name name :args args :result result) statements)
      result)))

(defun emit-ret (ifunc &optional (return-value nil))
  "Emit a new ret statement with an optional return-value."
  (check-type ifunc ifunc)
  (check-type return-value value)
  (with-slots (statements) ifunc
    (vector-push-extend (make-instance 'ret :return-value return-value) statements)))

(defun emit-alloc (ifunc size)
  "Emit a new alloc statement. Returns the resulting value."
  (check-type ifunc ifunc)
  (check-type size keyword)
  (with-slots (allocs) ifunc
    (let ((result (new-temp-var ifunc :ptr)))
      (vector-push-extend (make-instance 'alloc :size size :result result) allocs)
      result)))

(defun emit-mem-load (ifunc addr mem-type)
  "Emit a new mem-load statement."
  (check-type ifunc ifunc)
  (check-type addr var)
  (with-slots (statements) ifunc
    (let ((result (new-temp-var ifunc mem-type)))
      (vector-push-extend (make-instance 'mem-load :result result :addr addr) statements)
      result)))

(defun emit-mem-store (ifunc value addr)
  "Emit a new mem-store statement."
  (check-type ifunc ifunc)
  (check-type value value)
  (check-type addr var)
  (with-slots (statements) ifunc
    (vector-push-extend (make-instance 'mem-store :value value :addr addr) statements)))

(defun emit-jump (ifunc target)
  "Emit a new jump instruction."
  (check-type ifunc ifunc)
  (check-type target string)
  (with-slots (statements) ifunc
    (vector-push-extend (make-instance 'jump :target target) statements)))

(defun emit-jump-zero (ifunc target jump-cond)
  "Emit a new jump instruction."
  (check-type ifunc ifunc)
  (check-type target string)
  (check-type jump-cond value)
  (with-slots (statements) ifunc
    (vector-push-extend (make-instance 'jump-zero :target target :jump-cond jump-cond) statements)))

(defun emit-unary (ifunc opcode arg1)
  "Emit a new mem-load statement."
  (check-type ifunc ifunc)
  (check-type opcode keyword)
  (check-type arg1 value)
  (with-slots (statements) ifunc
    (let ((result (new-temp-var ifunc (value-type arg1))))
      (vector-push-extend (make-instance 'unary :opcode opcode
                                                :arg1 arg1
                                                :result result)
                          statements)
      result)))

(defun emit-binary (ifunc opcode arg1 arg2)
  "Emit a new mem-load statement."
  (check-type ifunc ifunc)
  (check-type opcode keyword)
  (check-type arg1 value)
  (check-type arg2 value)
  (with-slots (statements) ifunc
    (let ((result (new-temp-var ifunc (value-type arg1))))
      (vector-push-extend (make-instance 'binary :opcode opcode
                                                :arg1 arg1
                                                :arg2 arg2
                                                :result result)
                          statements)
      result)))
