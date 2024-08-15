(in-package :clc)

(defun compiler-driver (source-code output-file &rest passes)
  "Run specific compiler passes. Supply pass names as keywords (e.g. :parser :check :print-ast :irgen
:codegen). Pass names must be supplied in the correct order."
  (loop with parser = (make-instance 'parser:parser :source source-code :source-file-name "test.c")
        with ast
        with ir
        for pass in passes do
          (case pass
            (:parser
             (setf ast (parser:parse parser)))
            (:check
             (ast:resolve-names ast)
             (if (ast:check-ast ast)
               (return-from compiler-driver)))
            (:print
             (ast:print-ast ast))
            (:irgen
             (setf ir (ast:irgen ast)))
            (:codegen
             (with-open-file (asm-code output-file :direction :output :if-exists :supersede)
               (codegen:output-assembly asm-code (codegen:codegen ir)))
               ;;(uiop:run-program (format nil "gcc ~a" output-file)))
             ;; (with-open-file (out output-file :direction :output :if-exists :supersede)
             ;;   (codegen:output-assembly out (codegen:codegen ir)))
             ))))

(defun main ()
  "Main entry point to compiler."
  (let ((source-code (alexandria:read-file-into-string "test.c")))
    (format t "~s" source-code)
    (compiler-driver source-code "test.s" :parser :check :print :irgen :codegen)))
