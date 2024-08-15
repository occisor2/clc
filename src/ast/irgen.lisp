;;;; AST to IRCode translation

(in-package :ast)

(defun irgen (compilation-unit)
  "Generate IR code."
  (check-type compilation-unit compilation-unit)
  (let ((functions (make-array 1 :adjustable t :fill-pointer 0)))
    (map 'vector #'(lambda (func)
                     (vector-push-extend (emit-ifunc func) functions))
         (functions compilation-unit))
    (let ((iprogram (make-instance 'ir:iprogram :functions functions)))
      (format t "~a" iprogram)
      iprogram)))

(defun ctype-to-ir-type (ctype)
  "Translate C types into IR types."
  (check-type ctype keyword)
  (case ctype
    (:void :void)
    (:int :i32)))

(defun emit-ifunc (function-node)
  "Translate FUNCTION-NODE into an IR `ifunc'."
  (check-type function-node function-node)
  (let* ((return-type (ctype-to-ir-type (return-type function-node)))
         (name (name (ident function-node)))
         (ifunc (ir:make-ifunc name return-type)))
    ;; add parameters to ifunc
    (map 'vector #'(lambda (param)
                     (emit-ircode param ifunc))
         (params function-node))
    ;; emit the body's code
    (emit-ircode (body function-node) ifunc)
    ;; Refer to a reference for more info; this extra return instruction is to handle undefined
    ;; behavior if a return statement is forgotten for non void functions as well as the special
    ;; case of main. It is optimized away later.
    (ir:emit-ret ifunc (ir:make-constant 0 :i32))
    ;; return new ifunc
    ifunc))

(defgeneric emit-ircode (node ifunc)
  (:documentation "Translate AST node into IR instructions and return result location."))

(defmethod emit-ircode ((node compound-node) (ifunc ir:ifunc))
  (map 'vector #'(lambda (node)
                   (emit-ircode node ifunc))
       (statements node)))

(defmethod emit-ircode ((node param-decl-node) (ifunc ir:ifunc))
  (let ((name (name (ident node)))
        (param-type (ctype-to-ir-type (decl-type node))))
    (setf (ir-addr node) (ir:add-parameter ifunc name param-type))))

(defmethod emit-ircode ((node return-node) (ifunc ir:ifunc))
  (ir:emit-ret ifunc (when (expr node)
                       (emit-ircode (expr node) ifunc))))

(defmethod emit-ircode ((node function-call-node) (ifunc ir:ifunc))
  (let ((args (make-array (length (args node)) :adjustable t :fill-pointer 0)))
    (map 'vector #'(lambda (arg)
                     (vector-push-extend (emit-ircode arg ifunc) args))
         (args node))
    (ir:emit-call ifunc (name (ident node)) (ctype-to-ir-type (ctype node)) args)))

(defmethod emit-ircode ((node var-decl-node) (ifunc ir:ifunc))
  (setf (ir-addr node) (ir:emit-alloc ifunc (ctype-to-ir-type (decl-type node)))))

(defmethod emit-ircode ((node if-node) (ifunc ir:ifunc))
  (let ((cond-result (emit-ircode (test node) ifunc)))
    (if (else node)
        (let ((else-label (ir:make-label-name ifunc "else"))
              (end-label (ir:make-label-name ifunc "if-end")))
          (ir:emit-jump-zero ifunc else-label cond-result)
          (emit-ircode (body node) ifunc)
          (ir:emit-jump ifunc end-label)
          (ir:emit-label ifunc else-label)
          (emit-ircode (else node) ifunc)
          (ir:emit-label ifunc end-label))
        (let ((end-label (ir:make-label-name ifunc "if-end")))
          (ir:emit-jump-zero ifunc end-label cond-result)
          (emit-ircode (body node) ifunc)
          (ir:emit-label ifunc end-label)))))

(defmethod emit-ircode ((node ident-node) (ifunc ir:ifunc))
  ;; rvalues need to be loaded to access their value. lvalues are handled specially in the
  ;; binary-node method.
  (if (rvalue node)
      (ir:emit-mem-load ifunc (ir-addr (decl-ref node)) (ctype-to-ir-type (ctype node)))))

(defmethod emit-ircode ((node int-node) (ifunc ir:ifunc))
  (ir:make-constant (int node) :i32))

(defmethod emit-ircode ((node unary-node) (ifunc ir:ifunc))
  (let ((left (emit-ircode (left node) ifunc)))
    (ir:emit-unary ifunc (case (kind node)
                           (:neg :neg)
                           (:logic-not :not)
                           (otherwise (error "not a valid unary operator")))
                   left)))

(defmethod emit-ircode ((node binary-node) (ifunc ir:ifunc))
  (let ((left (emit-ircode (left node) ifunc))
        (right (emit-ircode (right node) ifunc)))
    (case (kind node)
      (:assign
       (let ((lvalue-addr (ir-addr (decl-ref (left node)))))
         (ir:emit-mem-store ifunc right lvalue-addr)
         ;; assignments evaluate to the value assigned. This is needed when assignments are used as
         ;; expressions instead as statements.
         right))
      ((:add :sub :mul :div)
       (ir:emit-binary ifunc (kind node) left right))
      ((:equal :not-equal :less :less-equal :greater :greater-equal)
       (ir:emit-compare ifunc (kind node) left right))
      (otherwise (error "not a valid binary operator")))))
