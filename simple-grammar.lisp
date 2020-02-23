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

(defun generate (phrase)
  "Generate a random sentence or phrase. "
  (cond ((listp phrase) ; is the phrase a list?
         (mappend #'generate phrase)) ; run generate on list elts then append 'em
        ((rewrites phrase) ; if it's not a list, can we rewrite it?
         (generate (random-elt (rewrites phrase)))) ; pick a random elt & rewrite
        (t (list phrase)))) ; it must be a word so list it
