;;;; Package declarations

(defpackage :clc
  (:use :cl))

(defpackage :diagnostic
  (:use :cl)
  (:nicknames :diag)
  (:export
    #:diagnostic
    #:level
    #:line
    #:message))

(defpackage :lexer
  (:use :cl)
  (:export
    #:token
    #:kind
    #:value
    #:lexer
    #:lex))

(defpackage :ir
  (:use :cl)
  (:export
    #:make-ifunc
    #:make-constant
    #:make-label-name
    #:add-parameter
    #:emit-label
    #:emit-call
    #:emit-ret
    #:emit-alloc
    #:emit-mem-load
    #:emit-mem-store
    #:emit-jump
    #:emit-jump-zero
    #:emit-unary
    #:emit-binary
    #:iprogram
    #:functions
    #:ifunc
    #:name
    #:return-type
    #:params
    #:allocs
    #:statements
    #:value
    #:value-type
    #:var
    #:arg
    #:arg-num
    #:constant
    #:statement
    #:label
    #:call
    #:args
    #:ret
    #:return-value
    #:alloc
    #:size
    #:result
    #:mem-load
    #:addr
    #:mem-store
    #:jump
    #:target
    #:jump-zero
    #:jump-cond
    #:unary
    #:opcode
    #:arg1
    #:binary
    #:arg2))

(defpackage :codegen
  (:use :cl)
  (:export
    #:codegen
    #:output-assembly))

(defpackage :ast
  (:use :cl)
  ;; For some reason these don't get exported by the snippet at the top of ast.lisp
  (:export
   ;;#:compilation-unit
           ;; #:unary-node
           ;; #:binary-node
           ;; #:compound-node
           ;; #:ident-node
           ;; #:var-decl-node
           ;; #:param-decl-node
           ;; #:function-node
           ;; #:int
           ;; #:kind
           ;; #:ident
           ;; #:return-node
    ;; #:expr
    #:print-ast
    #:resolve-names
    #:check-ast
    #:irgen))

(defpackage :parser
  (:use :cl)
  (:export
    #:parser
    #:parse))
