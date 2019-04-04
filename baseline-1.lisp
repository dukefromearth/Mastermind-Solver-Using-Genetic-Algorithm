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
  (declare (notinline fold))
  (labels
      ((aux (x ls acc)
  	 ;(break)
  	 (cond
	   ((endp ls) acc)
	   (T (aux x (rest ls) (append acc (list (cons  x (first ls)))))))))
    (aux (first ls) (rest ls) '())))



(defun baseline-1-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (let (possible)
    ;; If the set of all possible codes hasn't been generated, generate only once and store in global variable
    (if (null *all-possible-codes*)
	;; Permuate a subset for every possible combination
	(progn
	  (setf possible (permutate colors board))
	  ;; Recursively fold subsets until one last level remains (special condition: final-fold)
	  (setf possible (loop for item in possible collect (fold item)))
	  ;; Perform final fold and possible will be left with all possible permutations
	  (loop for x from 1 to (- board 2)
	     do (setf possible (loop for item in possible append (final-fold item))))
	  (setf *all-possible-codes* possible)))

    ;; Remember previously generated list
    (setf possible *all-possible-codes*)
    
    ;; Remove already used guesses from the front of list

    (if (not (null (member *previous-guess* *all-possible-codes*)))
	      (setf possible (rest (member *previous-guess* *all-possible-codes*))))

    ;; Store guess in gobal variable so it retains the information out of scope
    (setf *previous-guess* (first possible))
    ;(print *previous-guess*)
    ;; finally return next guess in lexographical order:
    (first possible)))
