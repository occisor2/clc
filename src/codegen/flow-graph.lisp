;;;; Control flow grpah implementation

(in-package :codegen)

(declaim (optimize safety))
(defclass flow-graph ()
  ((nodes
    :accessor nodes
    :type hash-table
    :documentation "Maps block-id's to blocks.")))

(defmethod initialize-instance :after ((obj flow-graph) &key)
  (with-slots (nodes) obj
    (setf nodes (make-hash-table :test 'eql))))

(defmethod print-object ((obj flow-graph) out)
  (with-slots (nodes) obj
    (format out "Control Flow Graph:~%")
    (loop for node being the hash-values of nodes do
      (format out "~a~%" node))))

(defmacro do-flow-graph ((node graph) &body body)
  `(loop for ,node being the hash-values of (nodes ,graph) do
         ,@body))

(deftype block-id () '(or integer (member :entry :exit)))

(declaim (optimize safety))
(defclass basic-block ()
  ((id
    :initarg :id
    :accessor id
    :type block-id)
   (instructions
    :accessor instructions
    :type (array instruction))
   (predecessors
    :accessor predecessors
    :type (array block-id))
   (successors
    :accessor successors
    :type (array block-id))
   (lives
    :initform nil
    :accessor lives
    :type list
    :documentation "List of current live registers at the block's start.")))

(defmethod initialize-instance :after ((obj basic-block) &key)
  (with-slots (instructions predecessors successors) obj
    (setf instructions (make-array 0 :adjustable t :fill-pointer 0)
          predecessors (make-array 0 :adjustable t :fill-pointer 0)
          successors (make-array 0 :adjustable t :fill-pointer 0))))

(defmethod print-object ((obj basic-block) out)
  (with-slots (id instructions predecessors successors lives) obj
    (format out "Basic Block ~a:~%" id)
    (format out "Predecessors: ~{~a~^, ~}~%" (coerce predecessors 'list))
    (format out "Successors: ~{~a~^, ~}~%" (coerce successors 'list))
    (format out "Lives: ~{~a~^, ~}~%" lives)
    ;;(format out "Instructions:~%~{~a~%~}" (coerce instructions 'list))
    (format out "Instructions:~%")
    (loop for instruction across instructions do
      (format out "~a [lives ~{~a~^, ~}]~%" instruction (lives instruction)))))

(defun make-flow-graph (instructions)
  "Create a new flow graph out of INSTRUCTIONS."
  (check-type instructions (array instruction))
  (let ((graph (make-instance 'flow-graph)))
    (setf (gethash :entry (nodes graph)) (make-basic-block :entry))
    (loop for node across (partition instructions) do
      (setf (gethash (id node) (nodes graph)) node))
    (setf (gethash :exit (nodes graph)) (make-basic-block :exit))
    (add-all-edges graph)
    graph))

(defun node-count (graph)
  "Get the number of nodes in GRAPH."
  (hash-table-count (nodes graph)))

(defun get-node-by-id (graph id)
  "Search for a block by its id."
  (check-type graph flow-graph)
  (check-type id block-id)
  (gethash id (nodes graph)))

(defun get-node-by-label (graph label)
  "Search for a block by its beginning label."
  (check-type graph flow-graph)
  (check-type label string)
  (loop for node being the hash-values of (nodes graph) do
    (unless (or (eq (id node) :entry)
                (eq (id node) :exit)
                (block-empty-p node)
                (not (typep (elt (instructions node) 0) 'label)))
      (let ((label-name (name (elt (instructions node) 0))))
        (when (equal label label-name)
          (return-from get-node-by-label node))))))

(defun make-basic-block (id)
  (check-type id block-id)
  (make-instance 'basic-block :id id))

(defun block-empty-p (basic-block)
  (check-type basic-block basic-block)
  (zerop (length (instructions basic-block))))

(defun add-to-block (basic-block instruction)
  (check-type basic-block basic-block)
  (check-type instruction instruction)
  (vector-push-extend instruction (instructions basic-block)))

(defun last-instruction (basic-block)
  (check-type basic-block basic-block)
  (unless (zerop (length (instructions basic-block)))
    (elt (instructions basic-block) (1- (length (instructions basic-block))))))

(defun partition (instructions)
  "Partition INSTRUCTIONS into `basic-block''s."
  (check-type instructions (array instruction))
  (let* ((blocks (make-array 0 :adjustable t :fill-pointer 0))
         (block-id 0)
         (current (make-basic-block block-id)))
    (loop for instruction across instructions do
      (typecase instruction
        (label
         (unless (block-empty-p current)
           (vector-push-extend current blocks))     ; push the last block
         (incf block-id)                            ; increment block id counter
         (setf current (make-basic-block block-id)) ; create a new block
         (add-to-block current instruction))        ; add the label to the block
        ((or jmp jmpcc ret)
         (add-to-block current instruction)
         (vector-push-extend current blocks)
         (incf block-id)
         (setf current (make-basic-block block-id)))
        (otherwise
         (add-to-block current instruction))))
    (unless (block-empty-p current)
      (vector-push-extend current blocks))
    blocks))

(defun add-edge (node1 node2)
  "Add an edge between NODE1 and NODE2 with NODE1 as the predecessor."
  (check-type node1 basic-block)
  (check-type node2 basic-block)
  (vector-push-extend (id node2) (successors node1))
  (vector-push-extend (id node1) (predecessors node2)))

(defun add-all-edges (graph)
  "Connect edges between nodes in GRAPH."
  (add-edge (get-node-by-id graph :entry) (get-node-by-id graph 0))
  (loop for node being the hash-values of (nodes graph) do
    (unless (or (eq (id node) :entry) (eq (id node) :exit))
      (let ((next-id (if (= (id node) (1- (node-count graph)))
                         :exit
                         (1+ (id node))))
            (instruction (last-instruction node)))
        (typecase instruction
          (ret
           (add-edge node (get-node-by-id graph :exit)))
          (jmp
           (let ((target-node (get-node-by-label graph (target instruction))))
             (add-edge node target-node)))
          (jmpcc
           (let ((target-node (get-node-by-label graph (target instruction))))
             (add-edge node target-node)
             (add-edge node (get-node-by-id graph next-id))))
          (otherwise
           (add-edge node (get-node-by-id graph next-id))))))))
