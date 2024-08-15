;;;; Name resolution pass of the compiler

(in-package :ast)

(defclass c-symbol ()
  ((decl-ref
    :initarg :decl-ref
    :accessor decl-ref
    :type (or var-decl-node param-decl-node function-node)
    :documentation "The declaration this symbol refers to.")))

(defun c-symbol-var-p (c-symbol)
  (check-type c-symbol c-symbol)
  (or (typep (decl-ref c-symbol) 'var-decl-node)
      (typep (decl-ref c-symbol) 'param-decl-node)))

(defun c-symbol-func-p (c-symbol)
  (check-type c-symbol c-symbol)
  (typep (decl-ref c-symbol) 'function-node))

(defclass symbol-table ()
  ((table
    :initarg :table
    :accessor table
    :type hash-table)
   (parent
    :initarg :parent
    :accessor parent
    :type (or symbol-table null)))
  (:default-initargs :table (make-hash-table :test 'equal)
                     :parent nil))

(defun resolve-names (ast)
  "Verify that all identifiers have been declared and annotate identifier nodes."
  (name-resolution ast (make-instance 'symbol-table)))

(defun symbol-exists-p (table name)
  (check-type table symbol-table)
  (check-type name string)
  (cond ((nth-value 1 (gethash name (table table))) t)
        ((null (parent table)) nil)
        (t (symbol-exists-p (parent table) name))))

(defun symbol-in-scope-p (table name)
  (check-type table symbol-table)
  (check-type name string)
  (nth-value 1 (gethash name (table table))))

(defun get-symbol (table name)
  (check-type table symbol-table)
  (check-type name string)
  (gethash name (table table))
  (let ((c-symbol (gethash name (table table))))
    (cond (c-symbol c-symbol)
          ((null (parent table)) nil)
          (t (get-symbol (parent table) name)))))

(defun register-symbol (table name decl-node)
  (check-type table symbol-table)
  (check-type name string)
  (check-type decl-node (or var-decl-node param-decl-node function-node))
  (setf (gethash name (table table))
        (make-instance 'c-symbol :decl-ref decl-node)))

(define-condition name-error (error)
  ((message
    :initarg :message
    :accessor message))
  (:report (lambda (condition stream)
             (format stream "Name Error: ~a" (message condition)))))

(defgeneric name-resolution (node table)
  (:documentation "Check that all identifiers under NODE have definitions and assign them types."))

(defmethod name-resolution ((node compilation-unit) (table symbol-table))
  ;; ignore global variable scope for now
  (map 'vector #'(lambda (n)
                   (name-resolution n table))
       (functions node)))

(defmethod name-resolution ((node function-node) (table symbol-table))
  ;; make sure a function with the same name hasn't already been defined
  (when (symbol-exists-p table (name (ident node)))
    (error 'name-error
           :message (format nil "function '~a' has already been defined" (name (ident node)))))
  ;; add the function to the symbol table
  (register-symbol table (name (ident node)) node)
  ;; create a new scope for the parameters, which will be shadowed by the body's scope
  (let ((new-scope (make-instance 'symbol-table :parent table)))
    (map 'vector #'(lambda (n)
                     (name-resolution n new-scope))
         (params node))
    (name-resolution (body node) new-scope)))

(defmethod name-resolution ((node compound-node) (table symbol-table))
  (let ((new-scope (make-instance 'symbol-table :parent table)))
    (map 'vector #'(lambda (n)
                     (name-resolution n new-scope))
         (statements node))))

(defmethod name-resolution ((node var-decl-node) (table symbol-table))
  (let ((var-name (name (ident node))))
    ;; make sure variable hasn't been declared already in current scope
    (if (symbol-in-scope-p table var-name)
        (error 'name-error
               :message (format nil "'~a' has already been defined" var-name))
        (progn
          (register-symbol table var-name node)
          (name-resolution (ident node) table)))))

(defmethod name-resolution ((node param-decl-node) (table symbol-table))
  (let ((var-name (name (ident node))))
    ;; make sure variable hasn't been declared already in current scope
    (if (symbol-in-scope-p table var-name)
        (error 'name-error
               :message (format nil "'~a' has already been defined" var-name))
        (progn
          (register-symbol table var-name node)
          (name-resolution (ident node) table)))))

(defmethod name-resolution ((node ident-node) (table symbol-table))
  (if (symbol-exists-p table (name node))
      (let* ((c-sym (get-symbol table (name node)))
             (ctype (decl-type (decl-ref c-sym))))
        ;; Make sure the symbol is a variable. This method is never called to check function call
        ;; identifiers.
        (unless (c-symbol-var-p c-sym)
          (error 'name-error
                 :message (format nil "'~a' has not been defined" (name node))))
        (setf (ctype node) ctype)
        (setf (decl-ref node) (decl-ref c-sym)))
      (error 'name-error
             :message (format nil "'~a' has not been defined" (name node)))))

(defmethod name-resolution ((node function-call-node) (table symbol-table))
  (if (symbol-exists-p table (name (ident node)))
      (let ((c-sym (get-symbol table (name (ident node)))))
        ;; verify that the symbol is actually a function and not a variable
        (if (c-symbol-func-p c-sym)
            (let ((ctype (return-type (decl-ref c-sym))))
              ;; Set the type of the call identifier and link to function declaration. The semantics
              ;; pass will set the type of the whole call expression after verifying it.
              (setf (ctype (ident node)) ctype)
              (setf (decl-ref (ident node)) (decl-ref c-sym))
              ;; resolve any names in the argument expressions
              (map 'vector #'(lambda (arg)
                               (name-resolution arg table))
                   (args node)))
            (error 'name-error
                   :message (format nil "'~a' is not a function" (name (ident node))))))
      (error 'name-error
             :message (format nil "'~a' is not a function" (name (ident node))))))

(defmethod name-resolution ((node return-node) (table symbol-table))
  (unless (null (expr node))
    (name-resolution (expr node) table)))

(defmethod name-resolution ((node binary-node) (table symbol-table))
  (name-resolution (left node) table)
  (name-resolution (right node) table))

(defmethod name-resolution ((node unary-node) (table symbol-table))
  (name-resolution (left node) table))

(defmethod name-resolution ((node if-node) (table symbol-table))
  (name-resolution (test node) table)
  (name-resolution (body node) table)
  (when (else node)
    (name-resolution (else node) table)))

;; some expressions (e.g literals) don't need checking so just use an empty stub, so previous
;; methods don't have to check that their children aren't literals.
(defmethod name-resolution ((node expression) (table symbol-table)))
