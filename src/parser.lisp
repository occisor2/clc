;;;; Parse token stream to create an AST

(in-package :parser)

(defparameter *precedences*
  '(:none
    :assign
    :or
    :and
    :equal
    :compare
    :term
    :factor
    :unary
    :call
    :primary)
  "Precedences of different operators for expression parser.")

;; As more functionality is added to the parser, some of these tokens will be assigned new rules.
(defparameter *parse-rules*
  (make-hash:make-hash :size 47 :test 'eq
                       :initial-contents '(:eof           (nil nil :none)
                                           :error         (nil nil :none)
                                           :left-paren    (parse-grouping parse-call :call)
                                           :right-paren   (nil nil :none)
                                           :left-brace    (nil nil :none)
                                           :right-brace   (nil nil :none)
                                           :left-bracket  (nil nil :none)
                                           :right-bracket (nil nil :none)
                                           :comma         (nil nil :none)
                                           :dot           (nil nil :none)
                                           :question      (nil nil :none)
                                           :slash         (nil parse-binary :factor)
                                           :star          (nil parse-binary :factor)
                                           :colon         (nil nil :none)
                                           :semicolon     (nil nil :none)
                                           :minus         (parse-unary parse-binary :term)
                                           :minus-minus   (nil nil :none)
                                           :plus          (nil parse-binary :term)
                                           :plus-plus     (nil nil :none)
                                           :bang          (parse-unary nil :none)
                                           :bang-equal    (nil parse-binary :equal)
                                           :equal         (nil parse-binary :assign)
                                           :equal-equal   (nil parse-binary :equal)
                                           :greater       (nil parse-binary :compare)
                                           :greater-equal (nil parse-binary :compare)
                                           :less          (nil parse-binary :compare)
                                           :less-equal    (nil parse-binary :compare)
                                           :ampersand     (nil nil :none)
                                           :and           (nil nil :none)
                                           :bar           (nil nil :none)
                                           :or            (nil nil :none)
                                           :left-shift    (nil nil :none)
                                           :right-shift   (nil nil :none)
                                           :caret         (nil nil :none)
                                           :tilde         (nil nil :none)
                                           :identifier    (parse-expr-ident nil :primary)
                                           :string        (nil nil :none)
                                           :number        (parse-number nil :primary)
                                           :int           (nil nil :none)
                                           :char          (nil nil :none)
                                           :void          (nil nil :none)
                                           :if            (nil nil :none)
                                           :else          (nil nil :none)
                                           :for           (nil nil :none)
                                           :while         (nil nil :none)
                                           :return        (nil nil :none)
                                           :struct        (nil nil :none)))
  "Parsing rules for the epression parser.")

(defun rule (token)
  "Return the parser rule for TOKEN in `*parser-rules*'."
  (check-type token lexer:token)
  (gethash (lexer:kind token) *parse-rules*))

(defun prefix (token)
  "Return the associated prefix parsing function for TOKEN."
  (check-type token lexer:token)
  (nth 0 (rule token)))

(defun infix (token)
  "Return the associated infix parsing function for TOKEN."
  (check-type token lexer:token)
  (nth 1 (rule token)))

(defun prec-to-num (prec)
  "Convert a precedence keyword into an integer."
  (check-type prec keyword)
  (position prec *precedences*))

(defun precedence (token)
  "Return the associated precedence of TOKEN."
  (check-type token lexer:token)
  (prec-to-num (nth 2 (rule token))))

(defun right-associative-p (token)
  "Predicate for testing whether a token is right associative."
  (check-type token lexer:token)
  (eql (lexer:kind token) :equal))

(define-condition syntax-error (error)
  ((message
    :initarg :message
    :initform ""
    :reader message
    :documentation "Error message that describes the error.")
   (bad-token
    :initarg :bad-token
    :reader bad-token
    :documentation "Token which caused the error to be raised."))
  (:report (lambda (condition stream)
             (format stream "Syntax Error: ~a" (message condition))))
  (:documentation "Error the parser uses to signal a syntax error was encountered."))

(defclass parser ()
  ((source-file-name
    :initarg :source-file-name
    :type string
    :documentation "File name of source file.")
   (source
    :initarg :source
    :type string
    :documentation "Source code string for parsing.")
   (lexer
    :type lexer:lexer)
   (current
    :type lexer:token)
   (previous
    :type lexer:token)))

(defmethod initialize-instance :after ((obj parser) &key)
  (with-slots (source lexer current previous) obj
    (setf lexer (make-instance 'lexer:lexer :source source)
          current (make-instance 'lexer:token :kind :eof)
          previous current)))

(defun parse (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (next parser)
    (loop with functions = (make-array 1 :fill-pointer 0 :adjustable t)
          until (eql (lexer:kind current) :eof)
          do (vector-push-extend (parse-function parser) functions)
          finally (return (make-instance 'ast:compilation-unit :functions functions)))))

(defun next (parser)
  "Return the next token in the token stream."
  (check-type parser parser)
  (with-slots (previous current lexer) parser
    (setf previous current)
    (setf current (lexer:lex lexer))
    (format t "~s~%" current)
    current))

(defun consume (parser token-type error-msg)
  "Consume the current token and return it. If the type is not TOKEN-TYPE signal an error."
  (check-type parser parser)
  (check-type token-type keyword)
  (check-type error-msg string)
  (with-slots (previous current) parser
    (if (eql (lexer:kind current) token-type)
        (progn
          (next parser)
          previous)
        (error 'syntax-error
               :message error-msg
               :bad-token current))))

(defun match (parser token-type)
  "Consume the current token if it matches TOKEN-TYPE."
  (check-type parser parser)
  (check-type token-type keyword)
  (with-slots (current) parser
    (when (eql (lexer:kind current) token-type)
      (next parser)
      t)))

(defun check (parser token-type)
  "Check if the current token matches TOKEN-TYPE."
  (check-type parser parser)
  (check-type token-type keyword)
  (with-slots (current) parser
    (eql (lexer:kind current) token-type)))

(defmacro case-consume ((error-msg) parser &body body)
  "Test current token against a list of possible values. On a match, return token and move to
 the next token. If no match is found, signal a syntax error."
  (alexandria:once-only (error-msg parser)
    `(check-type ,error-msg string)
    `(check-type ,parser parser)
    `(with-slots (previous current) ,parser
       (case (lexer:kind current)
         ,@(mapcar #'(lambda (x)
                       (list x `(progn
                                  (next ,parser)
                                  previous)))
            body)
         (otherwise (error 'syntax-error
                           :message ,error-msg
                           :bad-token current))))))

(defun parse-ident (parser)
  (with-slots (previous) parser
    (consume parser :identifier "expected identifier")
    (make-instance 'ast:ident-node :name (lexer:value previous))))

(defun parse-param-decl (parser)
  (check-type parser parser)
  (with-slots (previous current) parser
    (let ((decl-type (lexer:kind (case-consume ("expected type specifier") parser
                                   :int)))
          (var-name (parse-ident parser)))
      (make-instance 'ast:param-decl-node :decl-type decl-type
                                          :ident var-name))))

(defun parse-params (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (consume parser :left-paren "expected parameter list")
    ;; check that an empty parameter list contains the void keyword (e.g void func (void) not void
    ;; func ())
    (unless (and (check parser :void)
                 (progn
                   (next parser)
                   (consume parser :right-paren "expected closing paranthesis")))
      ;; if the parameter list isn't empty, start parsing parameters
      (loop with params = (make-array 0 :fill-pointer 0 :adjustable t)
            until (check parser :right-paren)
            do
               (vector-push-extend (parse-param-decl parser) params)
               ;; check for trailing comma
               (when (and (match parser :comma)
                          (check parser :right-paren))
                 (error 'syntax-error
                        :message "expected parameter declaration"
                        :bad-token current))
            finally
               (consume parser :right-paren "expected closing parenthesis")
               ;; if no parameters were found, the list is completely empty which is not allowed
               (when (zerop (length params))
                 (error 'syntax-error
                        :message "empty parameter lists must be denoted with 'void'"
                        :bad-token current))
               (return params)))))

(defun parse-function (parser)
  (check-type parser parser)
  (let ((return-type (lexer:kind (case-consume ("expected return type") parser
                                   :int))))
    (next parser) ; skip past identifier because that parse function uses the `previous' token
    (let ((func-name (parse-expr-ident parser))
          (params (parse-params parser)))
      (make-instance 'ast:function-node :ident func-name
                                        :body (parse-compound parser)
                                        :params params
                                        :return-type return-type))))

(defun parse-return (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (consume parser :return "expected return statement")
    (let ((statement (make-instance 'ast:return-node
                                    :expr (if (eql (lexer:kind current) :semicolon)
                                              nil
                                              (parse-expression parser)))))
      (consume parser :semicolon "expected semicolon")
      statement)))

(defun parse-compound (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (consume parser :left-brace "expected opening brace")
    (loop with statements = (make-array 1 :fill-pointer 0 :adjustable t)
          until (or (eql (lexer:kind current) :right-brace)
                    (eql (lexer:kind current) :eof))
          for statement = (parse-statement parser)
          do (when statement ; don't store null statements
               (vector-push-extend statement statements))
          finally (consume parser :right-brace "unbalanced braces")
                  (return (make-instance 'ast:compound-node :statements statements)))))

(defun parse-statement (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (case (lexer:kind current)
      (:eof nil)
      (:int (parse-var-decl parser))
      (:left-brace (parse-compound parser))
      (:return (parse-return parser))
      (:if (parse-if parser))
      (:semicolon (next parser) nil) ; skip empty statement, which is allowed
      (otherwise
       (let ((expr (parse-expression parser)))
         (consume parser :semicolon "expected semicolon after statement")
         expr)))))

(defun parse-if (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (next parser)
    (consume parser :left-paren "expected opening parenthesis")
    (let ((expr (parse-expression parser)))
      (consume parser :right-paren "unbalanced parentheses")
      (let ((body (parse-compound parser))
            ;; check if there is an else clause
            (else (when (match parser :else)
                    (parse-compound parser))))
        (make-instance 'ast:if-node :test expr
                                    :body body
                                    :else else)))))

;; intialization will be handled later
(defun parse-var-decl (parser)
  (check-type parser parser)
  (with-slots (current) parser
    (let ((decl-type (lexer:kind current)) ; get the type decl that was matched in `parse-statement'
          (var-name (progn
                      (next parser)
                      (parse-ident parser))))
      (consume parser :semicolon "expected semicolon after statement")
      (make-instance 'ast:var-decl-node :ident var-name
                                        :decl-type decl-type))))

(defun parse-expression (parser)
  (check-type parser parser)
  (parse-precedence parser (prec-to-num :none)))

(defun parse-precedence (parser prec)
  (check-type parser parser)
  (check-type prec integer)
  (with-slots (current previous) parser
    (next parser)
    (unless (prefix previous)
      (error 'syntax-error
             :message (format nil "'~s' is not a unary operator" (lexer:value previous))
             :bad-token previous))
    (loop with left = (funcall (prefix previous) parser)
          while (> (precedence current) prec) do
            (next parser)
            (unless (infix previous)
              (error 'syntax-error
                     :message (format nil "'~s' is not a binary operator" (lexer:value previous))
                     :bad-token previous))
            (setf left (funcall (infix previous) parser left))
          finally (return left))))

(defun parse-grouping (parser)
  (check-type parser parser)
  (let ((expr (parse-expression parser)))
    (consume parser :right-paren "unbalanced parentheses")
    expr))

(defun parse-number (parser)
  (check-type parser parser)
  (with-slots (previous) parser
    (make-instance 'ast:int-node :int (parse-integer (lexer:value previous)))))

(defun parse-expr-ident (parser)
  (check-type parser parser)
  (with-slots (previous) parser
    (make-instance 'ast:ident-node :name (lexer:value previous))))

(defun parse-call (parser left)
  (check-type parser parser)
  (loop with args = (make-array 0 :adjustable t :fill-pointer 0)
        until (check parser :right-paren)
        do
           (vector-push-extend (parse-expression parser) args)
           ;; check for trailing comma
           (when (and (match parser :comma)
                      (check parser :right-paren))
             (error 'syntax-error
                    :message "expected expression"
                    :bad-token (slot-value parser 'current)))
        finally
           (consume parser :right-paren "expected closing parenthesis")
           (return (make-instance 'ast:function-call-node :ident left :args args))))

(defun token-to-unary-op (token)
  "Returns the associated unary operation for TOKEN as a keyword."
  (check-type token lexer:token)
  (case (lexer:kind token)
    (:minus :neg)
    (:bang :logic-not)))

(defun parse-unary (parser)
  (check-type parser parser)
  (with-slots (previous) parser
    (make-instance 'ast:unary-node
                   :kind (token-to-unary-op previous)
                   :left (parse-precedence parser (prec-to-num :unary)))))

(defun token-to-binary-op (token)
  "Returns the associated binary operation for TOKEN as a keyword."
  (check-type token lexer:token)
  (case (lexer:kind token)
    (:minus :sub)
    (:plus :add)
    (:slash :div)
    (:star :mul)
    (:equal :assign)
    (:bang-equal :not-equal)
    (:equal-equal :equal)
    (:less :less)
    (:less-equal :less-equal)
    (:greater :greater)
    (:greater-equal :greater-equal)))

(defun parse-binary (parser left)
  (check-type parser parser)
  (check-type left ast:expression)
  (with-slots (previous) parser
    (make-instance 'ast:binary-node
                   :kind (token-to-binary-op previous)
                   :left left
                   :right (parse-precedence parser (if (right-associative-p previous)
                                                       (1- (precedence previous))
                                                       (precedence previous))))))
