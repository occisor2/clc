;;;; Lexer for chunking source code into tokens

(in-package :lexer)

(defparameter *token-types*
  '(:eof
    :error
    :left-paren
    :right-paren
    :left-brace
    :right-brace
    :left-bracket
    :right-bracket
    :comma
    :dot
    :question
    :slash
    :star
    :colon
    :semicolon
    :minus
    :minus-minus
    :plus
    :plus-plus
    :bang
    :bang-equal
    :equal
    :equal-equal
    :greater
    :greater-equal
    :less
    :less-equal
    :ampersand
    :and ; a lot shorter than ampersand-ampersand
    :bar ; bitwise or
    :or  ; logical or
    :left-shift
    :right-shift
    :caret ; ^ bitwise xor
    :tilde ; ~ bitwise not
    ;; literals
    :identifier
    :string
    :number
    ;; keywords
    :int
    :char
    :void
    :if
    :else
    :for
    :while
    :return
    :struct))

(defparameter *keywords-table*
  (make-hash:make-hash :size 6 :test 'equal
                       :initial-contents '("int"    :int
                                           "char"   :char
                                           "void"   :void
                                           "if"     :if
                                           "else"   :else
                                           "for"    :for
                                           "while"  :while
                                           "return" :return
                                           "struct" :struct)))

(defun keyword-p (str)
  "Test if STR is a C keyword."
  (check-type str string)
  (nth-value 1 (gethash str *keywords-table*)))

(defclass token ()
  ((kind
    :initform :eof
    :initarg :kind
    :reader kind
    :documentation "Token type")
   (value
    :initform ""
    :initarg :text
    :reader value
    :documentation "Text that matched the token in source code.")
   (line
    :initform 1
    :initarg :line
    :reader line
    :documentation "Line in source code the token was matched.")))

(defmethod print-object ((obj token) out)
  (print-unreadable-object (obj out :type t)
    (format out "~s ~s: ~s" (line obj) (kind obj) (value obj))))

(defclass lexer ()
  ((source
    :initform ""
    :initarg :source
    :documentation "Source code to scan.")
   (start
    :initform 0
    :documentation "Start of the current token.")
   (current
    :initform 0
    :documentation "The current character being scanned.")
   (line
    :initform 1
    :documentation "Current line in the source file.")))

(defun lex (lexer)
  "Returns the current token."
  (with-slots (start current) lexer
    (skip-whitespace lexer)
    (setf start current)
    (if (emptyp lexer)
        (make-token lexer :eof)
        (let ((c (next lexer)))
          (case c
            (#\( (make-token lexer :left-paren))
            (#\) (make-token lexer :right-paren))
            (#\{ (make-token lexer :left-brace))
            (#\} (make-token lexer :right-brace))
            (#\[ (make-token lexer :left-bracket))
            (#\] (make-token lexer :right-bracket))
            (#\, (make-token lexer :comma))
            (#\. (make-token lexer :dot))
            (#\? (make-token lexer :question))
            (#\/ (make-token lexer :slash))
            (#\* (make-token lexer :star))
            (#\: (make-token lexer :colon))
            (#\; (make-token lexer :semicolon))
            (#\~ (make-token lexer :tilde))
            (#\^ (make-token lexer :caret))
            (#\- (make-token lexer (if (match lexer #\-) :minus-minus :minus)))
            (#\+ (make-token lexer (if (match lexer #\+) :plus-plus :plus)))
            (#\! (make-token lexer (if (match lexer #\=) :bang-equal :bang)))
            (#\= (make-token lexer (if (match lexer #\=) :equal-equal :equal)))
            (#\> (make-token lexer (cond ((match lexer #\=) :greater-equal)
                                         ((match lexer #\>) :right-shift)
                                         (t :greater))))
            (#\< (make-token lexer (cond ((match lexer #\=) :less-equal)
                                         ((match lexer #\<) :left-shift)
                                         (t :less))))
            (#\& (make-token lexer (if (match lexer #\&) :and :ampersand)))
            (#\| (make-token lexer (if (match lexer #\|) :or :bar)))
            (otherwise
             (cond ((digit-char-p c)
                    (putback lexer)
                    (lex-number lexer))
                   ((or (char= c #\_) (alpha-char-p c))
                    (putback lexer)
                    (lex-ident lexer))
                   ((char= c #\")
                    (lex-string lexer))
                   (t (make-error lexer "unknown token")))))))))

(defun valid-ident-char-p (c)
  "Tests whether character C is a valid character to be in an identifier."
  (check-type c character)
  (or (char= c #\_)
      (alphanumericp c)))

(defun lex-ident (lexer)
  (check-type lexer lexer)
  (with-slots (source start current) lexer
    (loop for c = (peek lexer)
          while (and (not (emptyp lexer))
                     (valid-ident-char-p c))
          do (next lexer))
    ;; If the identifer is a keyword look up the type otherwise return identifier token
    (make-token lexer (let ((ident (subseq source start current)))
                        (if (keyword-p ident)
                            (gethash ident *keywords-table*)
                            :identifier)))))

(defun lex-number (lexer)
  (check-type lexer lexer)
  (loop for c = (peek lexer)
        while (and (not (emptyp lexer))
                   (digit-char-p c))
        do (next lexer))
  (make-token lexer :number))

(defun lex-string (lexer)
  (check-type lexer lexer)
  (loop for c = (peek lexer)
        until (or (emptyp lexer)
                  (char= c #\"))
        do (next lexer))
  (if (emptyp lexer)
      (make-error lexer "unterminated string")
      (progn
        (next lexer) ; include the terminating '"'
        (make-token lexer :string))))

(defun match (lexer expected)
  "If the current character matches return T and advance otherwise return NIL."
  (check-type lexer lexer)
  (check-type expected character)
  (cond ((emptyp lexer) nil)
        ((char= expected (peek lexer)) (next lexer) t)
        (t nil)))

(defun make-token (lexer kind)
  "Create token with type KIND at the current location."
  (check-type lexer lexer)
  (check-type kind keyword)
  (with-slots (source start current line) lexer
    (make-instance 'token :kind kind
                          :text (subseq source start current)
                          :line line)))

(defun make-error (lexer msg)
  "Create an error token with message MSG."
  (check-type lexer lexer)
  (check-type msg string)
  (with-slots (source line) lexer
    (make-instance 'token :kind :error
                          :text msg
                          :line line)))

(defun emptyp (lexer)
  "Tests if the end of the file contents have been reached."
  (check-type lexer lexer)
  (with-slots (source current) lexer
    (>= current (length source))))

(defun putback (lexer)
  "Put the last character back."
  (check-type lexer lexer)
  (with-slots (current) lexer
    (decf current)))

(defun peek (lexer)
  "Returns the current character."
  (check-type lexer lexer)
  (with-slots (source current) lexer
    (if (emptyp lexer)
        nil
        (char source current))))

(defun next (lexer)
  "Returns the current character then advances to the next."
  (check-type lexer lexer)
  (with-slots (current) lexer
    (let ((c (peek lexer)))
      (incf current)
      c)))

(defun whitespace-p (c)
  (check-type c character)
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Newline)
      (char= c #\Return)))

(defun skip-whitespace (lexer)
  "Consume characters until a non-whitespace character is reached."
  (check-type lexer lexer)
  (with-slots (line) lexer
    (loop for c = (peek lexer)
          while (and (not (emptyp lexer))
                     (whitespace-p c))
          do (when (char= c #\Newline)
               (incf line))
             (next lexer))))
