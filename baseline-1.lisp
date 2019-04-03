;; Team:         Moonlight Pink Flamingoes
;; Name of file: baseline-1.lisp
;; Description:  Baseline-1 agent creates a set of all possible combinations of
;;               specified colors of size board.

;; The agent will need some form of memory
(defvar *previous-guess* nil)

;; Function performs the special condition final fold needed to create a usable possibility set.
;; Essentially map folded segment over final subset
(defun final-fold (set)
  (loop for subset in set
     append (loop for item in (member-if #'listp subset)
	   collect (append (remove-if #'listp subset) item))))

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
  (labels
      ((aux (x ls acc)
  	 ;(break)
  	 (cond
  	   ((endp ls) acc)
	   (T (aux x (rest ls) (append acc (list (cons  x (first ls)))))))))
    (aux (first ls) (rest ls) '())))



(defun baseline-1-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (let (possible)
    ;; Permuate a subset for every possible combination
    (setf possible (permutate colors board))

    ;; Recursively fold subsets until one last level remains (special condition: final-fold)
    (setf possible (loop for item in possible collect (fold item)))

    ;; Perform final fold and possible will be left with all possible permutations
    (setf possible (loop for item in possible append (final-fold item)))

    ;; Remove already used guesses from the front of list
    (setf possible (remove last-response (member *previous-guess* possible)))

    ;; Store guess in gobal variable so it retains the information out of scope
    (setf *previous-guess* (first possible))
    
    ;; finally return next guess in lexographical order:
    (first possible)))
