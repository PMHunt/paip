(defun mappend (fn the-list)
  "Apply a fn to each item in the list then append resulting items"
  (apply #'append (mapcar fn the-list)))


(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (Article) (Noun)))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun Adj* ()
  (if (= (random 2) 0)
      nil
      (append (Adj) (Adj*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

;;; (defun noun-phrase () (append (Article) (Adj*) (Noun) (PP*))) ; clashes
(defun PP () (append (Prep) (noun-phrase)))
(defun Adj () (one-of '(big little blue green adiabatic)))
(defun Prep () (one-of '(to in by with on)))

;;; --- new approach
;; Data-driven  approach is make it easy to rewrite the grammar
;; Abstract away Lisp details and focus on problem expression

(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defvar *grammar* *simple-grammar*
  "The grammar used by generate.  Initially, this is
  *simple-grammar*, but we can switch to other grammars.")

(defun rule-lhs (rule)
  "The left hand side of a rewrite rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rewrite rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate2 (phrase)
  "Generate a random sentence or phrase. This version answers Ex 2.1. "
  (let ((choices nil)) ; initialise choices for this scope
    (cond ((listp phrase)               ; is the phrase a list?
           (mappend #'generate2 phrase)) ; run generate on list elts then append 'em
          ((setf choices (rewrites phrase)) ; if it's not a list, can we rewrite it as one?
           (generate2 (random-elt choices))) ; run generate vs random elt
          (t (list phrase)))))

(defun generate3 (phrase)
  "Generate a random sentence or phrase. This version answers Ex 2.2. "
  (cond ((listp phrase)               ; is the phrase a list?
         (mappend #'generate2 phrase)) ; run generate on list elts then append 'em
        ((not-terminal-p phrase) ; if it's not a list, can we rewrite it as one?
         (generate2 (random-elt (rewrites phrase)))) ; run generate vs random elt
        (t (list phrase))))

(defun not-terminal-p (category)
  "True iff this is a rewritable category, but not for list or terminal node"
  (not (null (rewrites category))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase  This version makes a tree instead of list. "
  (cond ((listp phrase)               ; is the phrase a list?
         (mapcar #'generate-tree phrase)) ; run generate on list elts
        ((not-terminal-p phrase) ; if it's not a list, can we rewrite it as one?
         (cons phrase (generate-tree (random-elt (rewrites phrase))))) ; run generate vs random elt
        (t (list phrase))))
