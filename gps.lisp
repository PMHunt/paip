;;; Simple version of Simon's General Problem Solver (GPS) after Norvig

(defvar *state* nil "The world is all that is the case")

(defvar *ops* nil "Available operations for changing state")

(defstruct op "Template for an operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: move from current to goal state using ops"
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an applicable  op"
  (or (member goal *state*)
      (some #'apply-op
            (find-all goal *ops* :test #'appropriate-p))))

(defun appropriate-p (goal op)
  "Appropriate means, op is in the add list fr the goal"
  (member goal (op-add-list op)))

(defun apply-op (op)
  "Print message and update the *state* of the world if op applies"
  (when (every #'achieve (op-preconds op))
    (print (list 'executing (op-action op)))
    (setf *state* (set-difference *state* (op-del-list op)))
    (setf *state* (union *state* (op-add-list op)))))
