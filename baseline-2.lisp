;; Team: Moonlight Pink Flamingoes
;; Name of file: baseline-2.lisp
;; Description: Baseline-2 guesses every possibility in lexigraphical
;;              order unless it is inconsisitent with some previous response.


;; Generate a set of subsets of all possible combinations, result requires folding to be
;; considered usable. Functions is recursive/iterative.
(defun permutate (set length)
  (if (= length 0)
      (loop for item in set
	 collect item))
  (if (> length 0)
      (loop for item in set
	 collect (append (list item) (permutate set (- length 1))))))

(defun fold (ls)
  (labels
      ((aux (x ls acc)
  	 ;(break)
  	 (cond
  	   ((endp ls) acc)
	   (T (aux x (rest ls) (append acc (list (cons  x (first ls)))))))))
    (aux (first ls) (rest ls) '())))

(defun final-fold (set)
  (loop for subset in set
     append (loop for item in (member-if #'listp subset)
	       collect (append (remove-if #'listp subset) item))))


;; Takes a sequence (A A B) and removes any guess that contains any
;; For example
;; (prune-sequence '(A B) '((B C) (B A) (A B) (A A)))
;; ((B C) (B A)
(defun prune-sequence (sequence ls)
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

(defun unique-items (sequence)
  (let (unique)
    (loop for item in sequence
       if (not (member item unique :test #'equal))
	 collect item into unique
       finally
	 (return unique))))

(defun generate-permutations (set length)
  (let (output)
    ;; copying permuation code from baseline 1
    (setf output (permutate set length))
    (setf output (loop for item in output collect (fold item)))
    (setf output (loop for item in output append (final-fold item)))))

;; memory is cheap but time is expensive
(defvar *previous-responses* nil)
;; most recent guess
(defvar *last-guess* nil)
;; list of previous guesses
(defvar *previous-guesses* nil)
(defvar *permutations* nil)
(defvar *possible-colors* nil)

(defun baseline-2-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (cons last-response *previous-responses*)
  ;; set up permutations
  (if (endp *permutations*)
      (progn
	(setf *permutations* (generate-permutations (colors board)))
	(setf *possible-colors* colors)))
  (cond
    ;; On first turn
    ((endp last-response) ((cons (first *permutations*) *previous-guesses*)
			   (setf *last-guess* (first *permutations*))
			   (setf *permutations* (rest *premutations))))
    ;; if last response was (0 0)
    ;; remove those colors from the list of candidates and permute again
    ;; removing the guesses that we have already made
    ((and (= (nth 0 last-response) 0) (= (nth 1 last-response) 0))
     
     (setf *possible-colors* (set-difference *possible-colors* *last-guess*))
     (setf *permutations* (generate-permutations *possible-colors* board))
     (setf *permutations* (set-difference *permutations* *previous-guesses*))
     (cons (first *permutations*) *previous-guesses*)
     (setf *last-guess* (first *permutations*))
     (setf *permutations* (rest *permutations*)))

    ;; if last response was (x board) then the answer is some permutation of the last guess
    ((= (nth 1 last-response) board)
     (setf *possible-colors* (unique-items *last-guess*))
     (setf *permutations* (generate-permutations *possible-colors* board))
     (cons (first *permutations*) *previous-guesses*)
     (setf *last-guess* (first *permutations*))
     (setf *permutations* (rest *permutations*)))
    
    ;; else just guess the next permutation in the sequence
    (t  (cons (first *permutations*) *previous-guesses*)
	(setf *last-guess* (first *permutations*))
	(setf *permutations* (rest *permutations*))))

  *last-guess*)
     
     
     
    
			   



