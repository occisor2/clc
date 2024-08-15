;;;; Abstract syntax tree implementation

(in-package :ast)

;; export all symbols from the package
;; (let ((pack (find-package :ast)))
;;   (do-all-symbols (sym pack)
;;     (when (eql (symbol-package sym) pack)
;;       (export sym))))

(named-readtables:in-readtable cl-annot-revisit:at-syntax-readtable)

@cl-annot-revisit:export-class
(defclass compilation-unit ()
  ((functions
    :initarg :functions
    :accessor functions
    :type array
    :documentation "Array of function nodes")))

@cl-annot-revisit:export-class
(defclass node-base ()
  ((token-info
    :initarg :token-info
    :accessor token-info
    :type lexer:token
    :documentation "Source information of node via the original token.")
   (errors
    :initarg :errors
    :accessor errors
    :type array
    :documentation "Any errors picked up during ast passes."))
  (:default-initargs :errors (make-array 0 :fill-pointer 0 :adjustable t)))

;; Before the type checking pass, all nodes will default to NIL type.
@cl-annot-revisit:export-class
(defclass expression (node-base)
  ((ctype
    :initarg :ctype
    :accessor ctype
    :type (or keyword null)
    :documentation "C type of expression (e.g. int, char, etc.)"))
  (:default-initargs :ctype nil))

@cl-annot-revisit:export-class
(defclass ident-node (expression)
  ((name
    :initarg :name
    :accessor name
    :type string
    :documentation "Symbol name of the identifier.")
   (decl-ref
    :accessor decl-ref
    :initform nil
    :type (or var-decl-node null)
    :documentation "A reference to the declaration node that this identifier refers to.")
   (rvalue
    :initarg :rvalue
    :accessor rvalue
    :type boolean))
  (:default-initargs :name "" :rvalue t))

@cl-annot-revisit:export-class
(defclass int-node (expression)
  ((int
    :initarg :int
    :accessor int
    :type integer))
  (:default-initargs :int 0))

@cl-annot-revisit:export-class
(defclass unary-node (expression)
  ((kind
    :initarg :kind
    :accessor kind
    :type keyword
    :documentation "Type of unary operation.")
   (left
    :initarg :left
    :accessor left
    :type expression
    :documentation "Expression the operation is applied to.")))

@cl-annot-revisit:export-class
(defclass binary-node (expression)
  ((kind
    :initarg :kind
    :accessor kind
    :type keyword
    :documentation "Type of binary operation.")
   (left
    :initarg :left
    :accessor left
    :type expression)
   (right
    :initarg :right
    :accessor right
    :type expression)))

@cl-annot-revisit:export-class
(defclass compound-node (node-base)
  ((statements
    :initarg :statements
    :accessor statements
    :type array)))

@cl-annot-revisit:export-class
(defclass var-decl-node (node-base)
  ((decl-type
    :initarg :decl-type
    :accessor decl-type
    :type keyword
    :documentation "C type of the declaration (same as token value e.g. int).")
   (ident
    :initarg :ident
    :accessor ident
    :type ident-node)
   (init-value
    :initarg :init-value
    :accessor init-value
    :type (or expression null))
   (ir-addr
    :accessor ir-addr
    :initform nil
    :type (or ir:var null)
    :documentation "The IR register which contains the variable's address."))
  (:default-initargs :init-value nil))

@cl-annot-revisit:export-class
(defclass if-node (node-base)
  ((test
    :initarg :test
    :accessor test
    :type expression)
   (body
    :initarg :body
    :accessor body
    :type compound-node)
   (else
    :initarg :else
    :accessor else
    :type (or compound-node null)))
  (:default-initargs :else nil))

@cl-annot-revisit:export-class
(defclass param-decl-node (node-base)
  ((decl-type
    :initarg :decl-type
    :accessor decl-type
    :type keyword)
   (ident
    :initarg :ident
    :accessor ident
    :type ident-node)
   (ir-addr
    :accessor ir-addr
    :initform nil
    :type (or ir:var null)
    :documentation "The IR register which contains the variable's address.")))

@cl-annot-revisit:export-class
(defclass function-node (node-base)
  ((return-type
    :initarg :return-type
    :accessor return-type
    :type keyword)
   (ident
    :initarg :ident
    :accessor ident
    :type ident-node
    :documentation "Name of the function.")
   (params
    :initarg :params
    :accessor params
    :type array)
   (body
    :initarg :body
    :accessor body
    :type compound-node
    :documentation "Function body as an array of statements.")))

@cl-annot-revisit:export-class
(defclass return-node (node-base)
  ((expr
    :initarg :expr
    :accessor expr
    :type (or expression null)))
  (:default-initargs :expr nil))

@cl-annot-revisit:export-class
(defclass function-call-node (expression)
  ((ident
    :initarg :ident
    :accessor ident
    :type ident-node)
   (args
    :initarg :args
    :accessor args
    :type array)))
