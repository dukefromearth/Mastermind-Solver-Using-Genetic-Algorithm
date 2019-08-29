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


