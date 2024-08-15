(asdf:defsystem clc
  :author "Russell Smith <russell.smith7502@gmail.com>"
  :version "0.1.0"
  :description "A small C compiler in Common Lisp"
  :depends-on ("alexandria" "make-hash" "cl-annot-revisit")
  :serial t
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "diagnostic")
                 (:file "lexer")
                 (:module "ir"
                  :components ((:file "ir-ast")
                               (:file "print")
                               (:file "emit")))
                 (:module "ast"
                  :components ((:file "ast")
                               (:file "print")
                               (:file "name-resolution")
                               (:file "semantic-check")
                               (:file "irgen")
                               ))
                 (:file "parser")
                 (:module "codegen"
                  :components ((:file "asm-ast")
                               (:file "print")
                               (:file "flow-graph")
                               (:file "register-allocator")
                               (:file "fixup")
                               (:file "codegen")))
                 (:file "main")))))
