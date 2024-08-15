;;;; Print the AST in an easy to read format

(in-package :ast)

(defun print-ast (ast)
  (print-node ast 0))

(defun indent-line (level)
  (format t "~v@{~C~:*~}" level #\Space))

(defgeneric print-node (node indent)
  (:documentation "Print the contents of NODE as a tree indented to INDENT spaces."))

(defmethod print-node ((node compilation-unit) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "Compilation Unit~%")
  (map 'vector #'(lambda (n) (print-node n (+ indent 2))) (functions node)))

(defmethod print-node ((node compound-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "compound~%")
  (if (zerop (length (statements node)))
      (progn
        (format t "~v@{~C~:*~}" (+ indent 2) #\Space)
        (format t "empty~%"))
      (map 'vector #'(lambda (n) (print-node n (+ indent 2))) (statements node))))

(defmethod print-node ((node var-decl-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "variable declaration~%")
  (print-node (ident node) (+ indent 2)))

(defmethod print-node ((node ident-node) indent)
  (indent-line indent)
  (format t "identifier '~a' ~a~%" (name node) (ctype node)))

(defmethod print-node ((node int-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "int-lit '~a' ~a~%" (int node) (ctype node)))

(defmethod print-node ((node unary-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "unary op '~a' ~a~%" (kind node) (ctype node))
  (print-node (left node) (+ indent 2)))

(defmethod print-node ((node binary-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "binary op '~a' ~a~%" (kind node) (ctype node))
  (print-node (left node) (+ indent 2))
  (print-node (right node) (+ indent 2)))

(defmethod print-node ((node if-node) indent)
  (indent-line indent)
  (format t "if statement~%")
  (print-node (test node) (+ indent 2))
  (print-node (body node) (+ indent 2))
  (when (else node)
    (indent-line (+ indent 2))
    (format t "else~%")
    (print-node (else node) (+ indent 4))))

(defmethod print-node ((node return-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "return~%")
  (unless (null (expr node))
    (print-node (expr node) (+ indent 2))))

(defmethod print-node ((node param-decl-node) indent)
  (indent-line indent)
  (format t "param declaration~%")
  (print-node (ident node) (+ indent 2)))

(defmethod print-node ((node function-node) indent)
  (format t "~v@{~C~:*~}" indent #\Space)
  (format t "function '~a' ~a~%" (name (ident node)) (return-type node))
  (map 'vector #'(lambda (n)
                   (print-node n (+ indent 2)))
       (params node))
  (print-node (body node) (+ indent 2)))

(defmethod print-node ((node function-call-node) indent)
  (indent-line indent)
  (format t "function call '~a' ~a~%" (name (ident node)) (ctype node))
  (if (zerop (length (args node)))
      (progn
        (indent-line (+ indent 2))
        (format t "no arguments~%"))
      (map 'vector #'(lambda (arg)
                       (indent-line (+ indent 2))
                       (format t "argument~%")
                       (print-node arg (+ indent 4)))
           (args node))))
