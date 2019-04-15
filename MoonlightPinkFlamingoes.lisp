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

    (if (null (first possible))
	(progn
	  (setf possible '(F F F F))
	  (setf *all-possible-codes* nil)
	  possible)
	(progn
	  ;; Store guess in gobal variable so it retains the information out of scope
	  (setf *previous-guess* (first possible))
					;(print *previous-guess*)
	  ;; finally return next guess in lexographical order:
	  ;(print (first possible))
	  (first possible)))))







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
  (if (listp (first set))
  (loop for subset in set
     append (loop for item in (member-if #'listp subset)
	       collect (append (remove-if #'listp subset) item)))
  (loop for item in (member-if #'listp set)
collect (append (remove-if #'listp set) item))))


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
    (loop for x from 1 to (- length 2)
       do (setf output (loop for item in output append (final-fold item)))
       finally
	 (return output))))

;; memory is cheap but time is expensive
(defvar *previous-responses* nil)
;; most recent guess
(defvar *last-guess* nil)
;; list of previous guesses
(defvar *previous-guesses* nil)
(defvar *permutations* nil)
(defvar *possible-colors* nil)

;;print all variables and inspect

(defun baseline-2-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (cons last-response *previous-responses*)
  ;; set up permutations
  ;(format t "1")
  (if (endp *permutations*)
      (progn
	(setf *permutations* (generate-permutations colors board))
	(setf *possible-colors* colors)))
  ;(format t "2")
  (cond
    ;; On first turn
    ((endp last-response) (progn (cons (first *permutations*) *previous-guesses*)
			   (setf *last-guess* (first *permutations*))
			   (setf *permutations* (rest *permutations*))))
    ;; if last response was (0 0)
    ;; remove those colors from the list of candidates and permute again
    ;; removing the guesses that we have already made
    ((and (= (nth 0 last-response) 0) (= (nth 1 last-response) 0))

    ; (format t "3")
     (setf *possible-colors* (set-difference *possible-colors* *last-guess*))
     (setf *permutations* (generate-permutations *possible-colors* board))
     (setf *permutations* (set-difference *permutations* *previous-guesses*))
     (cons (first *permutations*) *previous-guesses*)
     (setf *last-guess* (first *permutations*))
     (setf *permutations* (rest *permutations*)))

    ;; if last response was (x board) then the answer is some permutation of the last guess
    ((= (nth 1 last-response) board)
     ;(format t "4")
     (setf *possible-colors* (unique-items *last-guess*))
     (setf *permutations* (generate-permutations *possible-colors* board))
     (cons (first *permutations*) *previous-guesses*)
     (setf *last-guess* (first *permutations*))
     (setf *permutations* (rest *permutations*)))
    
    ;; else just guess the next permutation in the sequence
    
    (t
     ;(format t "5")
     (cons (first *permutations*) *previous-guesses*)
     (setf *last-guess* (first *permutations*))
     (setf *permutations* (rest *permutations*))))
  ;(format t "previous-responses: ~a~%" *previous-responses*)
  ;(format t "last-guess: ~a~%" *last-guess*)
  ;(format t "previous-guesses: ~a~%" *previous-guesses*)
  ;(format t "possible-colors: ~a~%" *possible-colors*)

  *last-guess*)
     
     
     
    
			   



;; Team:         Moonlight Pink Flamingoes
;; Name of file: baseline-3.lisp
;; Description:  Baseline-3 agent first determines the number of each color peg,
;;               then generates permutations from the colors and their numbers.

;; Global variables to keep track of previous guesses and their results.
;;(defvar *previous-hints* nil)
;;(defvar *previous-guesses* nil)
(defvar *current-guess* 1)
(defvar *color-counts*)
(defvar *previous-guess*)

;;Given a number, convert it into the corresponding symbol.
(defun unspot (number)
  (case number
    (0 'A)
    (1 'B)
    (2 'C)
    (3 'D)
    (4 'E)
    (5 'F)
    (6 'G)
    (7 'H)
    (8 'I)
    (9 'J)
    (10 'K)
    (11 'L)
    (12 'M)
    (13 'N)
    (14 'O)
    (15 'P)
    (16 'Q)
    (17 'R)
    (18 'S)
    (19 'TT)
    (20 'U)
    (21 'V)
    (22 'W)
    (23 'X)
    (24 'Y)
    (25 'Z)))

;; Given a list of numbers, convert it into a list of corresponding symbols.
(defun num-to-symb (list)
  (loop for num in list
       collect (unspot num)))

;; Given a list of numbers, return the next permutation in lexicographic order.
(defun next-num-perm (list)
  (loop for i from 1 to (1- (length list))
     with suffix = (list (first list))
     with pivot = nil
     with prefix = nil
     with successor = nil
     do (if (>= (first suffix) (nth i list))
	    (setf suffix (cons (nth i list) suffix))
	    (progn
	      (setf prefix (append suffix prefix))
	      (setf pivot (first suffix))
	      (setf suffix (list (nth i list)))))
     finally (progn
	       (if (equal prefix nil)
		   (return (reverse list)))
	       (loop for element in suffix
		  for i from 0
		  when (> element pivot)
		  do (progn
		       (setf successor (nth i suffix))
		       (setf (nth i suffix) pivot)
		       (setf pivot successor)
		       (return)))
	       (return (append (reverse (cdr prefix)) (list pivot) suffix)))))

(defun baseline-3-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (cond ((equal last-response nil)  ; The first guess of all As
	 (progn
	   (setf *current-guess* 1)
	   (setf *color-counts* (make-array (length colors)))
	   (setf *current-guess* (1+ *current-guess*))
       ;;(print (make-list board :initial-element (nth (- *current-guess* 2) colors)))
	   (make-list board :initial-element (nth (- *current-guess* 2) colors))))
	
	((< *current-guess* (length colors))    ; subsequent single color guesses
	 (progn
	   (setf (aref *color-counts* (- (third last-response) 1)) (+ (first last-response) (second last-response)))
	   (setf *current-guess* (1+ *current-guess*))
	   ;;(print (make-list board :initial-element (nth (- *current-guess* 2) colors)))
	   (make-list board :initial-element (nth (- *current-guess* 2) colors))))
	
	((= *current-guess* (length colors))    ; the first permutation guess
	 (progn
	   (setf (aref *color-counts* (- (third last-response) 1)) (+ (first last-response) (second last-response)))
	   (setf (aref *color-counts* (third last-response)) (- board (reduce #'+ *color-counts*)))
	   (setf *current-guess* (1+ *current-guess*))
	   (setf *previous-guess* (append (loop for x from 0 to (- (length colors) 1)
	      for color in colors
	      when (> (aref *color-counts* x) 0)
	      append (loop for y from 0 to (1- (aref *color-counts* x))
			collect (spot color)))))
	   ;;(print (num-to-symb *previous-guess*))
	   (num-to-symb *previous-guess*)))
	
	(T  ; Subsequent permutation guesses
	 (progn
	   (setf *current-guess* (1+ *current-guess*))
	   (setf *previous-guess* (next-num-perm *previous-guess*))
	   ;;(print (num-to-symb *previous-guess*))
	   (num-to-symb *previous-guess*)))))


