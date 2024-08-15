;;;; Register allocator

(in-package :codegen)

(defparameter +max-spill-cost+ 1000000
  "Can't assign infinity as a value for spilling, so this will do.")

(deftype interf-id-type () '(or temp register))

(declaim (optimize safety))
(defclass interference-graph ()
  ((nodes
    :initarg :nodes
    :accessor nodes
    :type hash-table
    :documentation "Maps register names to graph nodes, which contain the register instance.")))

(defmacro do-interf-graph ((node graph) &body body)
  `(loop for ,node being the hash-values of (nodes ,graph) do
         ,@body))

(defmethod print-object ((obj interference-graph) out)
  (format out "Interference Graph~%")
  (do-interf-graph (node obj)
    (format out "~a~%" node)))

(declaim (optimize safety))
(defclass interference-node ()
  ((id
    :initarg :id
    :accessor id
    :type interf-id-type)
   (neighbors
    :initarg :neighbors
    :accessor neighbors
    :type list
    :documentation "List of neighboring node ids in the interference graph.")
   (spill-cost
    :initarg :spill-cost
    :accessor spill-cost
    :type integer)
   (color
    :initarg :color
    :accessor color
    :type (or integer null))
   (pruned-p
    :initarg :pruned-p
    :accessor pruned-p
    :type boolean))
  (:default-initargs :neighbors nil :spill-cost 0 :color nil :pruned-p nil))

(defmethod print-object ((obj interference-node) out)
  (with-slots (id neighbors spill-cost color pruned-p) obj
    (format out "Node id=~a, spill-cost=~a, color=~a, pruned-p=~a," id spill-cost color pruned-p)
    (format out " neighbors=[~{~a~^, ~}]" neighbors)))

(defun make-interf-node (register spill-cost)
  "Return a new interference node."
  (check-type register interf-id-type)
  (make-instance 'interference-node :id register :spill-cost spill-cost))

(defun get-interf-node-by-id (graph id)
  "Return node in GRAPH with ID."
  (gethash (name id) (nodes graph)))

(defun add-to-interf-graph (graph node)
  "Add NODE to GRAPH."
  (check-type graph interference-graph)
  (check-type node interference-node)
  (setf (gethash (name (id node)) (nodes graph)) node))

(defun in-interf-graph-p (graph register)
  "Predicate for testing whether REGISTER is in GRAPH."
  (check-type graph interference-graph)
  (check-type register operand)
  (when (typep register '(or temp register))
    (gethash (name register) (nodes graph))))

(defun unpruned-in-graph (graph)
  "Returns a list of all unpruned nodes in GRPAH."
  (check-type graph interference-graph)
  (loop for node being the hash-values of (nodes graph)
        unless (pruned-p node)
          collect node))

(defun unpruned-neighbors (graph node)
  "Returns a list of NODE's unpruned neighbors."
  (check-type graph interference-graph)
  (check-type node interference-node)
  (loop for node-id in (neighbors node)
        unless (pruned-p (get-interf-node-by-id graph node-id))
          collect (get-interf-node-by-id graph node-id)))

(defun operand-equal-p (op1 op2)
  "Predicate for comparing register operands. Two registers are equal if they have the same name."
  (check-type op1 operand)
  (check-type op2 operand)
  (cond ((and (typep op1 'register)
              (typep op2 'register))
         (equal (name op1) (name op2)))
        ((and (typep op1 'temp)
              (typep op2 'temp))
         (equal (name op1) (name op2)))
        (t nil)))

;;;; Gather temporary register pass

(defun inc-temp-use (temp-map temp)
  "Increment name's use count in TEMP-MAP. If NAME isn't in the map, add it."
  (check-type temp-map hash-table)
  (check-type temp temp)
  (unless (gethash (name temp) temp-map)
    (setf (gethash (name temp) temp-map) 0))
  (incf (gethash (name temp) temp-map)))

(defun add-temporaries (graph instructions)
  "Add all temporary registers in INSTRUCTIONS to GRAPH."
  (check-type graph interference-graph)
  (check-type instructions (array instruction))
  (let ((temp-map (make-hash-table :test 'equal)) ; map from temp names to counts
        (temps nil)) ; list of temporaries found (can't hash temp objects)
    ;; find every use of a temporary in instructions and update a counter everytime it's used
    (loop for instruction across instructions do
      (typecase instruction
        (mov
         (when (typep (source instruction) 'temp)
           (setf temps (adjoin (source instruction) temps :test #'operand-equal-p)) ; add to set of temps
           (inc-temp-use temp-map (source instruction)))                            ; increment count
         (when (typep (dest instruction) 'temp)
           (setf temps (adjoin (dest instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (dest instruction))))
        (cmp
         (when (typep (arg1 instruction) 'temp)
           (setf temps (adjoin (arg1 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg1 instruction)))
         (when (typep (arg2 instruction) 'temp)
           (setf temps (adjoin (arg2 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg2 instruction))))
        (idiv
         (when (typep (arg1 instruction) 'temp)
           (setf temps (adjoin (arg1 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg1 instruction))))
        (unary
         (when (typep (arg1 instruction) 'temp)
           (setf temps (adjoin (arg1 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg1 instruction))))
        (binary
         (when (typep (arg1 instruction) 'temp)
           (setf temps (adjoin (arg1 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg1 instruction)))
         (when (typep (arg2 instruction) 'temp)
           (setf temps (adjoin (arg2 instruction) temps :test #'operand-equal-p))
           (inc-temp-use temp-map (arg2 instruction))))))
    (loop for temp in temps do
      ;; use the use count of the temp as its spill cost
      (let ((spill-cost (gethash (name temp) temp-map)))
        (add-to-interf-graph graph (make-interf-node temp spill-cost))))))

;;;; Liveness analysis pass

(defun meet (graph basic-block)
  (check-type graph flow-graph)
  (check-type basic-block basic-block)
  (loop
    with live-registers = nil
    for successor-id across (successors basic-block) do
      (case successor-id
        (:exit (setf live-registers (adjoin (make-register :rax :32)
                                            live-registers
                                            :test #'operand-equal-p)))
        (:entry (error "malformed control-flow graph"))
        (otherwise
         (let ((successor (get-node-by-id graph successor-id)))
           (setf live-registers (union live-registers (lives successor) :test #'operand-equal-p)))))
    finally (return live-registers)))

(defun find-used-and-updated (instruction)
  (check-type instruction instruction)
  (typecase instruction
    (call (values
           ;; based on the argument count, figure out which registers are used for arguments
           (loop for arg-num from 0 to (1- (arg-count instruction))
                 while (< arg-num (length +argument-registers+)) ; only the first 6 registers
                 collect (let ((register-name (elt +argument-registers+ arg-num)))
                           (make-instance 'register :name register-name :size :32)))
           (list (make-register :rdi :32)
                 (make-register :rsi :32)
                 (make-register :rdx :32)
                 (make-register :rcx :32)
                 (make-register :r8 :32)
                 (make-register :r9 :32)
                 (make-register :rax :32))))
    (mov (values (list (source instruction)) ; used
                 (list (dest instruction)))) ; updated
    (cmp (values (list (arg1 instruction) (arg2 instruction))
                 nil))
    (cdq (values (list (make-register :rax :32))
                 (list (make-register :rdx :32))))
    (idiv (values (list (arg1 instruction)
                        (make-register :rax :32)
                        (make-register :rdx :32))
                  (list (make-register :rax :32)
                        (make-register :rdx :32))))
    (unary (values (list (arg1 instruction))
                   (list (arg1 instruction))))
    (binary (values (list (arg1 instruction) (arg2 instruction))
                    (list (arg2 instruction))))
    (otherwise
     (values nil nil))))

(defun transfer (basic-block end-lives)
  (check-type basic-block basic-block)
  (check-type end-lives list)
  (let ((current-lives end-lives))
    (loop for instruction across (reverse (instructions basic-block)) do
      (setf (lives instruction) current-lives)
      (multiple-value-bind (used updated) (find-used-and-updated instruction)
        (loop for var in updated do
          (when (or (typep var 'temp)
                    (typep var 'register))
            (setf current-lives (remove var current-lives :test #'operand-equal-p))))
        (loop for var in used do
          (when (or (typep var 'temp)
                    (typep var 'register))
            (setf current-lives (adjoin var current-lives :test #'operand-equal-p))))))
    (setf (lives basic-block) current-lives)))

(defun analyze-liveness (graph)
  (check-type graph flow-graph)
  (let ((worklist (loop for node being the hash-values of (nodes graph)
                        unless (or (eq (id node) :entry) (eq (id node) :exit))
                          collect node)))
    (loop until (null worklist) do
      (let* ((basic-block (pop worklist))
             (old-annotation (lives basic-block))
             (outgoing-registers (meet graph basic-block)))
        (transfer basic-block outgoing-registers)
        (unless (equal old-annotation (lives basic-block))
          (loop for predecessor-id across (predecessors basic-block) do
            (case predecessor-id
              (:entry nil)
              (:exit (error "malformed control-flow graph"))
              (otherwise
               (let ((predecessor (get-node-by-id graph predecessor-id)))
                 (unless (member predecessor worklist)
                   (push predecessor worklist)))))))))))

;;;; Interference graph generation pass

(defun make-base-interf-graph ()
  "Return a base interference graph containing all general hard registers."
  (let ((nodes-map (make-hash-table :test #'equal)))
    ;; add all hard registers
    (loop for reg-name in +general-registers+ do
      (setf (gethash reg-name nodes-map)
            (make-interf-node (make-register reg-name :32) +max-spill-cost+)))
    ;; have every hard register interfere with each other
    (loop for node being the hash-values of nodes-map do
      (loop for neighbor being the hash-values of nodes-map do
        (unless (operand-equal-p (id node) (id neighbor))
          (push (id neighbor) (neighbors node)))))
    (make-instance 'interference-graph :nodes nodes-map)))

(defun add-interf-edge (node1 node2)
  "Add an edge bewteen NODE1 and NODE2."
  (check-type node1 interference-node)
  (check-type node2 interference-node)
  (unless (member (id node2) (neighbors node1) :test #'operand-equal-p)
    (push (id node2) (neighbors node1))
    (push (id node1) (neighbors node2))))

(defun add-interf-edges (graph cfg)
  "Add edges between interfering registers in GRAPH."
  (check-type graph interference-graph)
  (check-type cfg flow-graph)
  (do-flow-graph (node cfg)
    (unless (or (eq (id node) :entry) (eq (id node) :exit))
      (loop for instruction across (instructions node) do
        (multiple-value-bind (used updated) (find-used-and-updated instruction)
          (declare (ignore used))
          (loop for live in (lives instruction) do
            (unless (and (typep instruction 'mov)
                         (operand-equal-p live (source instruction)))
              (loop for update in updated do
                (when (and (and (in-interf-graph-p graph live)
                                (in-interf-graph-p graph update))
                           (not (operand-equal-p live update)))
                  (add-interf-edge (get-interf-node-by-id graph live)
                                   (get-interf-node-by-id graph update)))))))))))

(defun make-interf-graph (instructions)
  "Return a new interference graph."
  (check-type instructions (array instruction))
  (let ((graph (make-base-interf-graph)))
    (add-temporaries graph instructions)
    (let ((cfg (make-flow-graph instructions)))
      (analyze-liveness cfg)
      (add-interf-edges graph cfg)
      (color-interf-graph graph)
      ;;(format t "~a~%" graph)
      graph)))

;;;; Graph coloring and mapping pass

(defun color-interf-graph (graph)
  (check-type graph interference-graph)
  (let ((remaining (unpruned-in-graph graph)))
    (when remaining
      (let ((chosen-node
              ;; search for a node to prune with fewer than k neighbors
              (loop for node in remaining do
                (let ((degree (length (unpruned-neighbors graph node))))
                  (when (< degree (length +general-registers+))
                    (return node))))))
        (unless chosen-node
          ;; if none was found, pick the one with the smallest spill / degree ratio
          (let ((best-spill-metric +max-spill-cost+))
            (loop for node in remaining do
              (let* ((degree (length (unpruned-neighbors graph node)))
                     (spill-metric (/ (spill-cost node) degree)))
                (when (< spill-metric best-spill-metric)
                  (setf chosen-node node)
                  (setf best-spill-metric spill-metric))))))
        (setf (pruned-p chosen-node) t) ; prune the chosen node
        ;; color the rest of the graph via recursion
        (color-interf-graph graph)
        ;; color this node
        (let ((colors (loop for n from 1 to (length +general-registers+) collect n)))
          (loop for neighbor-id in (neighbors chosen-node) do
            (let ((neighbor (get-interf-node-by-id graph neighbor-id)))
              (when (color neighbor)
                (setf colors (remove (color neighbor) colors)))))
          (unless (zerop (length colors))
            (setf (color chosen-node)
                  (if (member (name (id chosen-node)) +callee-saved-registers+)
                      (first (last colors))
                      (first colors)))
            (setf (pruned-p chosen-node) nil)))))))

(defun color-mapped-p (color-map color)
  "Predicate for testing if COLOR is mapped to a hard register in COLOR-MAP."
  (check-type color-map hash-table)
  (check-type color integer)
  (nth-value 1 (gethash color color-map)))

(defun build-register-map (graph)
  (check-type graph interference-graph)
  (let ((color-map (make-hash-table))
        (reg-map (make-hash-table :test #'equal))
        (callees-saved nil))
    ;; map colors to hard registers
    (do-interf-graph (node graph)
      (when (typep (id node) 'register)
        (setf (gethash (color node) color-map) (id node))))
    ;; map temporaries to hard registers
    (do-interf-graph (node graph)
      (when (typep (id node) 'temp)
        (when (color-mapped-p color-map (color node))
          (let ((register (gethash (color node) color-map)))
            (setf (gethash (name (id node)) reg-map) register)
            (when (member (name register) +callee-saved-registers+)
              (setf callees-saved (adjoin register callees-saved :test #'operand-equal-p)))))))
    ;; (format t "Register Map:~%")
    ;; (loop for reg being the hash-values of reg-map
    ;;       using (hash-key key)
    ;;       do
    ;;          (format t "~a, ~a~%" key reg))
    ;; (format t "~%")
    (values reg-map callees-saved)))

;;;; Instruction rewrite pass

(defun temp-colored-p (reg-map temp)
  "Predicate for testing whether a temp is in the register map."
  (check-type reg-map hash-table)
  (check-type temp temp)
  (nth-value 1 (gethash (name temp) reg-map)))

(defun handle-operand-register-replace (reg-map instruction operand-slot)
  "Handle replacing temporary in INSTRUCTION via OPERAND-SLOT."
  (check-type reg-map hash-table)
  (check-type instruction instruction)
  (check-type operand-slot symbol)
  (when (and (typep (slot-value instruction operand-slot) 'temp)
             (temp-colored-p reg-map (slot-value instruction operand-slot)))
    (setf (slot-value instruction operand-slot)
          (gethash (name (slot-value instruction operand-slot)) reg-map))))

(defun replace-with-registers (asm-func reg-map)
  "Replace temporary registers in INSTRUCTION with corresponding hard registers in REG-MAP."
  (check-type asm-func asm-func)
  (check-type reg-map hash-table)
  (loop for instruction across (instructions asm-func) do
    (typecase instruction
      (mov
       ;; rewrite the operand only if it's a temporary and it's in the map
       (handle-operand-register-replace reg-map instruction 'source)
       (handle-operand-register-replace reg-map instruction 'dest))
      (cmp
       (handle-operand-register-replace reg-map instruction 'arg1)
       (handle-operand-register-replace reg-map instruction 'arg2))
      (idiv
       (handle-operand-register-replace reg-map instruction 'arg1))
      (unary
       (handle-operand-register-replace reg-map instruction 'arg1))
      (binary
       (handle-operand-register-replace reg-map instruction 'arg1)
       (handle-operand-register-replace reg-map instruction 'arg2)))))

(defun spilled-p (spill-map temp)
  "Predicate to test if a temporary has already been assigned a spill address."
  (check-type spill-map hash-table)
  (check-type temp temp)
  (nth-value 1 (gethash (name temp) spill-map)))

(defun spill-temp (asm-func spill-map temp)
  "Add TEMP to SPILL-MAP."
  (check-type asm-func asm-func)
  (check-type spill-map hash-table)
  (check-type temp temp)
  (setf (gethash (name temp) spill-map)
        (make-stack (stack-size asm-func) :32))
  (incf (stack-size asm-func) 4))

(defun spill-addr (spill-map temp)
  "Get the stack address of spilled TEMP."
  (check-type spill-map hash-table)
  (check-type temp temp)
  (gethash (name temp) spill-map))

(defun handle-operand-spill (asm-func spill-map instruction operand-slot)
  "handle spilling operand in INSTRUCTION accessed via OPERAND-SLOT if necessary."
  (check-type asm-func asm-func)
  (check-type spill-map hash-table)
  (check-type instruction instruction)
  (check-type operand-slot symbol)
  (when (typep (slot-value instruction operand-slot) 'temp)
    (unless (spilled-p spill-map (slot-value instruction operand-slot))
      (spill-temp asm-func spill-map (slot-value instruction operand-slot)))
    (setf (slot-value instruction operand-slot) (spill-addr spill-map (slot-value instruction operand-slot)))))

(defun handle-spills (asm-func)
  "Spill any temporaries that were not assigned hard registers to the stack."
  (check-type asm-func asm-func)
  ;; map temporary register names to stack objects
  (let ((spill-map (make-hash-table :test 'equal)))
    (loop for instruction across (instructions asm-func) do
      (typecase instruction
        (mov
         ;; rewrite the operand only if it's a temporary
         (handle-operand-spill asm-func spill-map instruction 'source)
         (handle-operand-spill asm-func spill-map instruction 'dest))
        (cmp
         (handle-operand-spill asm-func spill-map instruction 'arg1)
         (handle-operand-spill asm-func spill-map instruction 'arg2))
        (idiv
         (handle-operand-spill asm-func spill-map instruction 'arg1))
        (unary
         (handle-operand-spill asm-func spill-map instruction 'arg1))
        (binary
         (handle-operand-spill asm-func spill-map instruction 'arg1)
         (handle-operand-spill asm-func spill-map instruction 'arg2))))))

(defun allocate-registers (asm-func)
  "Replace temporary registers in ASM-FUNC with hard registers and note any callee saved registers
used for later during code emition. Spilled registers are not replaced."
  (check-type asm-func asm-func)
  (let ((interf-graph (make-interf-graph (instructions asm-func))))
    (multiple-value-bind (reg-map callees-saved) (build-register-map interf-graph)
      (replace-with-registers asm-func reg-map)
      (handle-spills asm-func)
      (setf (saves asm-func) callees-saved)
      ;;(format t "~a~%" asm-func)
      )))
