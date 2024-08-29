;;;; Post code generation instruction fixups

(in-package :codegen)

(defun fixup-instructions (asm-program asm-func)
  "Fixes any illegal operand pairings in instructions caused by others passes."
  (check-type asm-program asm-program)
  (check-type asm-func asm-func)
  (let ((total-instructions (make-array 1 :adjustable t :fill-pointer 0)))
    (setf total-instructions (concatenate 'vector total-instructions (fixup-prologue asm-func)))
    (loop for instruction across (instructions asm-func) do
      (setf total-instructions
            (concatenate 'vector total-instructions (fixup-instruction asm-program asm-func instruction))))
    (setf (instructions asm-func) total-instructions)
    ;; clear label map to not clobber other functions
    (clrhash (label-map asm-program))))

(defun align-to-16 (n)
  "Round up N to nearest value divisible by 16."
  (check-type n integer)
  (if (zerop (rem n 16))
      n
      (- (+ n 16)
         (rem n 16))))

(defun calculate-stack (asm-func)
  "Calculate how many bytes to need to be added to the stack frame to keep it 16 byte aligned."
  (check-type asm-func asm-func)
  (with-slots (stack-size saves) asm-func
    (let* ((saves-bytes (* (length saves) 8))
           (total-bytes (+ saves-bytes stack-size))
           (adjusted (align-to-16 total-bytes)))
      (- adjusted saves-bytes))))

(defun fixup-prologue (asm-func)
  "Add the stack instructions that create the function prologue."
  (check-type asm-func asm-func)
  (let ((new-instructions (make-array 1 :adjustable t :fill-pointer 0))
        (stack-space (calculate-stack asm-func)))
    ;; create stack frame
    (vector-push-extend (make-instance 'push-stack :arg1 (make-register :rbp)
                                                   :size :64)
                        new-instructions)
    (vector-push-extend (make-instance 'mov :source (make-register :rsp)
                                            :dest (make-register :rbp)
                                            :size :64)
                        new-instructions)
    ;; allocate space for local variables if needed
    (unless (zerop stack-space)
      (vector-push-extend (make-instance 'binary :opcode :sub
                                                 :arg1 (make-immediate stack-space)
                                                 :arg2 (make-register :rsp)
                                                 :size :64)
                          new-instructions))
    ;; push callee saved registers used
    (loop for saved in (saves asm-func) do
      ;; The registers might not be set to 64 bit, but are required to be for push, so fix that just
      ;; in case. Don't change the size for other instructions though, just push.
      (vector-push-extend (make-instance 'push-stack :arg1 (make-register (name saved)) :size :64)
                          new-instructions))
    new-instructions))

(defun fixup-instruction (asm-program asm-func instruction)
  (typecase instruction
    (label (fixup-label asm-program instruction))
    (jmp (fixup-jmp asm-program instruction))
    (jmpcc (fixup-jmpcc asm-program instruction))
    (leave (fixup-leave asm-func))
    (mov (fixup-mov instruction))
    (cmp (fixup-cmp instruction))
    (binary (fixup-binary instruction))
    (otherwise (vector instruction))))

(defun both-operands-mem-p (op1 op2)
  (check-type op1 operand)
  (check-type op2 operand)
  (and (typep op1 'stack)
       (typep op2 'stack)))

(defun label-already-found-p (label-map label-name)
  (gethash label-name label-map))

(defun fixup-label (asm-program instruction)
  "Give each label a file local unique number name. IR generates function local names, which also
aren't always valid GAS symbol names."
  (if (label-already-found-p (label-map asm-program) (name instruction))
      (vector (make-instance 'label :name (gethash (name instruction) (label-map asm-program))))
      (let ((new-name (format nil ".L~d" (label-count asm-program))))
        ;; add label to map for future label references
        (setf (gethash (name instruction) (label-map asm-program)) new-name)
        (incf (label-count asm-program))
        (vector (make-instance 'label :name new-name)))))

(defun fixup-jmp (asm-program instruction)
  (if (label-already-found-p (label-map asm-program) (target instruction))
      (vector (make-instance 'jmp :target (gethash (target instruction) (label-map asm-program))))
      (let ((new-name (format nil ".L~d" (label-count asm-program))))
        ;; add label to map for future label references
        (setf (gethash (target instruction) (label-map asm-program)) new-name)
        (incf (label-count asm-program))
        (vector (make-instance 'jmp :target new-name)))))

(defun fixup-jmpcc (asm-program instruction)
  (if (label-already-found-p (label-map asm-program) (target instruction))
      (vector (make-instance 'jmpcc :target (gethash (target instruction) (label-map asm-program))
                                    :jump-cond (jump-cond instruction)))
      (let ((new-name (format nil ".L~d" (label-count asm-program))))
        ;; add label to map for future label references
        (setf (gethash (target instruction) (label-map asm-program)) new-name)
        (incf (label-count asm-program))
        (vector (make-instance 'jmpcc :target new-name
                                      :jump-cond (jump-cond instruction))))))

(defun fixup-leave (asm-func)
  "Cleanup any saved callee registers before stack teardown."
  (check-type asm-func asm-func)
  (let ((new-instructions (make-array 1 :adjustable t :fill-pointer 0)))
    ;; cleanup callee saved registers
    (loop for saved in (reverse (saves asm-func)) do
      (vector-push-extend (make-instance 'pop-stack :arg1 (make-register (name saved)) :size :64)
                          new-instructions))
    (vector-push-extend (make-instance 'leave) new-instructions)
    new-instructions))

(defun fixup-mov (instruction)
  (let ((new-instructions (make-array 1 :adjustable t :fill-pointer 0)))
    (cond ((both-operands-mem-p (source instruction) (dest instruction))
           (vector-push-extend (make-instance 'mov :source (source instruction)
                                                   :dest (make-register :r10))
                               new-instructions)
           (vector-push-extend (make-instance 'mov :source (make-register :r10)
                                                   :dest (dest instruction))
                               new-instructions))
          ;; if coalesced registers, don't return any instructions
          ((operand-equal-p (source instruction) (dest instruction)))
          (t (vector-push-extend instruction new-instructions)))
    new-instructions))

(defun fixup-cmp (instruction)
  (let ((new-instructions (make-array 1 :adjustable t :fill-pointer 0)))
    (cond ((both-operands-mem-p (arg1 instruction) (arg2 instruction))
           (vector-push-extend (make-instance 'mov :source (arg1 instruction)
                                                   :dest (make-register :r10))
                               new-instructions)
           (vector-push-extend (make-instance 'cmp :arg1 (make-register :r10)
                                                   :arg2 (arg2 instruction))
                               new-instructions))
          ((typep (arg2 instruction) 'immediate)
           (vector-push-extend (make-instance 'mov :source (arg2 instruction)
                                                   :dest (make-register :r11))
                               new-instructions)
           (vector-push-extend (make-instance 'cmp :arg1 (arg1 instruction)
                                                   :arg2 :r11)
                               new-instructions))
          (t (vector-push-extend instruction new-instructions)))
    new-instructions))

(defun fixup-binary (instruction)
  (let ((new-instructions (make-array 1 :adjustable t :fill-pointer 0)))
    (cond ((and (eq (opcode instruction) :imul)
                (typep (arg2 instruction) 'stack))
           (vector-push-extend (make-instance 'mov :source (arg2 instruction)
                                                   :dest (make-register :r11))
                               new-instructions)
           (vector-push-extend (make-instance 'binary :opcode :imul
                                                      :arg1 (arg1 instruction)
                                                      :arg2 (make-register :r11))
                               new-instructions)
           (vector-push-extend (make-instance 'mov :source (make-register :r11)
                                                   :dest (arg2 instruction))
                               new-instructions))
          ((both-operands-mem-p (arg1 instruction) (arg2 instruction))
           (vector-push-extend (make-instance 'mov :source (arg1 instruction)
                                                   :dest (make-register :r10))
                               new-instructions)
           (vector-push-extend (make-instance 'binary :opcode (opcode instruction)
                                                      :arg1 (make-register :r10)
                                                      :arg2 (arg2 instruction))
                               new-instructions))
          (t (vector-push-extend instruction new-instructions)))
    new-instructions))
