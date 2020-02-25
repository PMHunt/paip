;;; Simple version of Simon's General Problem Solver (GPS) after Norvig

(defun find-all (item sequence &rest keyword-args
                 &key (test #'eql) test-not &allow-other-keys)
  "Find all those elements of sequence that match item,
  according to the keywords.  Doesn't alter sequence.
Norvig defines this in 3.19 and we're going to need it."
  (if test-not
      (apply #'remove item sequence
             :test-not (complement test-not) keyword-args)
      (apply #'remove item sequence
             :test (complement test) keyword-args)))

(defvar *state* nil "The world is all that is the case.   PEAS Environment")

(defvar *ops* nil "Available operations for changing state. PEAS Environment")

(defstruct op "Template for an operation"
           (action nil)
           (preconds nil)
           (add-list nil)
           (del-list nil))

(defun GPS (*state* goals *ops*)
  "General Problem Solver: move from current to goal state using ops"
  (if (every #'achieve goals) 'solved))

(defun achieve (goal)
  "A goal is achieved if it already holds, or if there is an applicable op
Argurably, this is the performance metric in Norvig's later PEAS model."
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

;;; let's set up a problem ----

(defparameter *school-ops*
  (list
   (make-op :action 'drive-son-to-school
            :preconds '(son-at-home car-works)
            :add-list '(son-at-school)
            :del-list '(son-at-home))
   (make-op :action 'shop-installs-battery
            :preconds '(car-needs-battery shop-knows-problem shop-has-money)
            :add-list '(car-works))
   (make-op :action 'tell-shop-problem
            :preconds '(in-communication-with-shop)
            :add-list '(shop-knows-problem))
   (make-op :action 'telephone-shop
            :preconds '(know-phone-number)
            :add-list '(in-communication-with-shop))
   (make-op :action 'look-up-number
            :preconds '(have-phone-book)
            :add-list '(know-phone-number))
   (make-op :action 'give-shop-money
            :preconds '(have-money)
            :add-list '(shop-has-money)
            :del-list '(have-money))))


;;; and try it

(gps '(son-at-home car-needs-battery have-money have-phone-book)
     '(son-at-school)
     *school-ops*)
