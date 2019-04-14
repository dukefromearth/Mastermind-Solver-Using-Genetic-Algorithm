;; Team:         Moonlight Pink Flamingoes
;; Name of file: baseline-1.lisp
;; Description:  Baseline-1 agent creates a set of all possible combinations of
;;               specified colors of size board.

;; The agent will need some form of memory
(defvar *previous-guess* nil)

;; Avoid recomputing same set every function call
(defvar *all-possible-codes* nil)

;; Function performs the special condition final fold needed to create a usable possibility set.
;; Essentially map folded segment over final subset
(defun final-fold (set)
  (if (listp (first set))
  (loop for subset in set
     append (loop for item in (member-if #'listp subset)
         collect (append (remove-if #'listp subset) item)))
  (loop for item in (member-if #'listp set)
     collect (append (remove-if #'listp set) item))))

;; Generate a set of subsets of all possible combinations, result requires folding to be
;; considered usable. Functions is recursive/iterative.
(defun permutate (set length)
  (if (= length 0)
      (loop for item in set
   collect item))
  (if (> length 0)
      (loop for item in set
   collect (append (list item) (permutate set (- length 1))))))

;; Merge first element with every sublist
;; Original list: '(A (A (A B) (B B) (C B)) (B (A B) (B B) (C B))))
;; fold 1 :    X   = A
;;             LS  = ((B (A B) (B B) (C B)))
;;             ACC = ((A A (A B) (B B) (C B)))
;; fold 2 :    X   = A
;;             LS  = NIL
;;             ACC = ((A A (A B) (B B) (C B)) (A B (A B) (B B) (C B)))
(defun fold (ls)
  ;(declare (notinline fold))
  (labels
      ((aux (x ls acc)
     ;(break)
     (cond
     ((endp ls) acc)
     (T (aux x (rest ls) (append acc (list (cons  x (first ls)))))))))
    (aux (first ls) (rest ls) '())))

;; Deletes any element that contains item 1 at position 1, item 2 at position 2
;; For example
;; (prune-sequence '(A B) '((B C) (B A) (A B) (A A)))
;; ((B C) (B A)
(defun remove-if-any-match (sequence ls)
  (labels
      ((aux (sequence ls acc)
	 (cond
	   ((endp ls) acc)
	   (T (loop for item in sequence
		    for x from 0
		    if (equal item (nth x (first ls)))
		 do (setf (first ls) nil))
	      (if (endp (first ls)) (aux sequence (rest ls) acc)
			(aux sequence (rest ls) (reverse (cons (first ls) acc))))))))
    (aux sequence ls '())))

(defun baseline-2-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let (possible)
    (if (null *all-possible-codes*)
	(progn
	  (setf possible (permutate colors board))
	  (setf possible (loop for item in possible collect (fold item)))
	  (loop for x from 1 to (- board 2)
	     do (setf possible (loop for item in possible append (final-fold item))))
	  (setf *all-possible-codes* possible))
	(progn
	  (setf possible *all-possible-codes*)
	  (setf *previous-guess* (first possible))
	  (if (and (not (null last-response)) (= 0 (+ (first last-response) (second last-response))))
	      (setf possible (remove-if-any-match (first possible) (rest possible))))
	  (setf *all-possible-codes* (rest possible))
	  ;(break)
	  (first possible)))))
