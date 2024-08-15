;;;; Verify semantic validity of the AST (e.g. type checks)

(in-package :ast)

(defclass context ()
  ((errors
    :initform nil
    :accessor errors
    :type boolean
    :documentation "Whether error(s) were signaled while traversing AST.")
   (return-type
    :initarg :return-type
    :accessor return-type
    :type keyword
    :documentation "Return type of the current function.")))

(defun check-ast (ast)
  "Run semantic checks on AST."
  (check-type ast compilation-unit)
  (let ((context (make-instance 'context)))
    ;; iterate over all the functions in the program and check them
    (map 'vector #'(lambda (func)
                       (setf (return-type context) (return-type func))
                       (semantic-check (body func) context))
         (functions ast))
    ;; return whether there were any errors
    (errors context)))

(defun report-semantic-error (node context message)
  (check-type node node-base)
  (check-type context context)
  (check-type message string)
  (let ((diagnostic (make-instance 'diag:diagnostic
                                   :level :error
                                   :line 0
                                   :message message)))
    (vector-push-extend diagnostic (errors node)) ; add to error list for print-ast
    (setf (errors context) t) ; signal there has been an error
    (format t "~a~%" diagnostic)))

(defgeneric semantic-check (node context)
  (:documentation "Run semantic checks."))

(defmethod semantic-check ((node if-node) (context context))
  (semantic-check (test node) context)
  (semantic-check (body node) context)
  (when (else node)
    (semantic-check (else node) context)))

(defmethod semantic-check ((node compound-node) (context context))
  (map 'vector #'(lambda (n)
                   (semantic-check n context))
       (statements node)))

(defmethod semantic-check ((node return-node) (context context))
  (if (expr node)
      (semantic-check (expr node) context)
      (report-semantic-error node context "empty return statement for non-void function")))

(defun correct-args-p (call-node)
  "Verify that the number of arguments passed matches the function definition."
  (let* ((decl-node (decl-ref (ident call-node)))
         (params (length (params decl-node)))
         (args (length (args call-node))))
    (= args params)))

(defmethod semantic-check ((node function-call-node) (context context))
  ;; verify the correct number of arguments are supplied
  (unless (correct-args-p node)
    (report-semantic-error node context "wrong number of arguments"))
  ;; set type of the call expression
  (setf (ctype node) (ctype (ident node)))
  ;; check call arguments
  (map 'vector #'(lambda (arg)
                   (semantic-check arg context))
       (args node)))

;; stub methods: until intialization is added, no checks need to be made on var-decl's
(defmethod semantic-check ((node var-decl-node) (context context)))

;; stub method: identifiers already have their typed assigned during name resolution.
(defmethod semantic-check ((node ident-node) (context context)))

(defmethod semantic-check ((node unary-node) (context context))
  (semantic-check (left node) context)
  ;; set the type of the expression
  (setf (ctype node) (ctype (left node))))

(defmethod semantic-check ((node binary-node) (context context))
  (semantic-check (left node) context)
  (semantic-check (right node) context)
  (let ((op-type (kind node)))
    ;; verify the left hand of an assignment is an lvalue
    (when (eq op-type :assign)
      (if (typep (left node) 'ident-node)
          (setf (rvalue (left node)) nil)
          (report-semantic-error (left node) context "not an lvalue")))
    ;; set the type of the expression
    (setf (ctype node) :int)))

(defmethod semantic-check ((node int-node) (context context))
  (setf (ctype node)
        ;; make sure literal values fit inside a type
        (cond ((<= (int node) #x7FFFFFFF) :int)
              (t (report-semantic-error node context "literal too big for type 'int'.")
                 :int))))
