
;; Team name:    Moonlight Pink Flamingoes
;; Date created: April 8th, 2019
;; Description: Genetic algorithm implementation for algorithm presented in the
;;              paper "Efficient solutions for Mastermind using genetic algorithms"
;;              by Berghman, Goossens, and Leus.

;;;;------------------------------------------
;;;; Symbol list
;;;;------------------------------------------
;; Pegs       -> pegs
;; Xi         -> bulls / black pegs
;; Yi         -> cows / white pegs
;; Ei         -> population at cycle i
;; (E hat i ) -> new population E at cycle i
;; c          -> candidate
;; h          -> generation counter
;; maxgen     -> max loops/generation cycles
;; maxsize    -> max size of population

;;;;------------------------------------------
;;;; Algorithm from paper
;;;;------------------------------------------
;; 1. Set i = 1
;; 2. Play fixed initial guess g1:
;; 3. Get response Xi and Yi;
;; 4. while Xi =/= Pegs do
;;   5. i = i + 1;
;;   6.Set (E hat i) = {} and h = 1;
;;   7. Initialize population;
;;   8. while (h <= maxgen AND |(E hat i)| <= maxsize) do
;;     9. Generate new population using crossover, mutation, inversion and permutation;
;;     10. Calculate fitness;
;;     11. Add eligible combinations to (E hat i) (if not yet contained in (E hat i));  
;;     12. h = h + 1;
;;   end while
;;   13. Play guess gi element of (E hat i);
;;   14. Get response Xi (bulls) Yi (cows)
;; end while

;;;;-----------------------------------------
;;;; Format
;;;;-----------------------------------------
;; Populant format (fitness (guess))
;; ...  once a populant is played, it turns into a guess
;; Guess format (black-pegs white-pegs (guess))

;;;; Fitness heuristic
;; 1. play all previous guesses as if they were secret code
;;    ... let X be the black pegs of previous guess being tested
;;    ... let Y be the white pegs of previous guess being tested
;;    ... let X'(c) be the black pegs of candidate c played against previous guess
;;    ... let Y'(c) be the white pegs of candidate c played against previous guess
;; 2. Difference between X'(c) with X, and Y'(c) with Y determines the quality of code
;; 3. The sum of all these differences determines fitness:
;;    ... a and b are weights, benchmarks on page 6 of paper for different tested values
;;    ... P is the number of positions, i is th enumbers of turns played (or guesses made)
;; FORMULA: fitness (c) = a * (summation of differences of c and previous guesses on black pegs) +
;;                            (summation of differences of c and previous guesses on white pegs) +
;;                        b * P(i - 1)


;; List keeps track of all guesses made
(defvar *guesses*)

;; Maximum number of generations run in generation-loop
(defvar *max-generations*)

;; Maximum size of each generated population
(defvar *max-size*)

;; Last generation made in previous turn
(defvar *previous-population*)

;; Size of initial population
(defvar *population-size*)

;; Original number of colors before modification
(defvar *number-of-colors-initial*)

;; List containing usable colors (genes)
(defvar *colors*)

;; Number of pegs used in each guess
(defvar *board*)

;; weight-a used in fitness function
(defvar *weight-a*)

;; weight-b used in fitness function
(defvar *weight-b*)

;; Number of turns played
(defvar *turns-played*)

;; Constant for 10% of max-size
(defvar *10-percent-of-size*)

;; Constant for 90% of max-size
(defvar *90-percent-of-size*)

;; Constant for 50% of max-size
(defvar *50-percent-of-size*)

;; Flag used to signal use of a few initial guesses to satisfy SCSA
(defvar *SCSA-constraints*)


;; Generate a population of specified size at random
(defun initialize-population ()
  (declare (optimize (speed 3) (safety 0)))
  (let (population candidate)
    (declare (optimize (speed 3) (safety 0)))
    (loop until (= (length population) *population-size*)
       do (setf candidate (create-gene-sequence))
       when (not (member (second candidate) population :test #'equal :key #'second))
       do (setf population (append population (list candidate)))
       and do (setf population (remove-duplicate-candidates population))
       finally (return population))))

;; Returns fitness value, does not calculate, this is used in sorting operations
(defun fitness (candidate)
  (declare (optimize (speed 3) (safety 0)))
  (first candidate))

;; Choose random gene
(defun mutation ()
  (declare (optimize (speed 3) (safety 0)))
  (nth (random (length *colors*)) *colors*))

;; Low chance of invervsion, swaps two genes at random
(defun inversion (offspring prob)
  (declare (optimize (speed 3) (safety 0)))
  (let ((random-spot1 (random (length offspring)))
	(random-spot2 (random (length offspring))))
    (declare (optimize (speed 3) (safety 0)))
    (if (>= prob 990)
	(setf (nth random-spot1 offspring) (nth random-spot2 offspring)))
    offspring))

;; Mate two guesses and produce an offspring using crossover
(defun mate (parent1 parent2)
  (declare (optimize (speed 3) (safety 0)))
  (let (prob)
    (declare (optimize (speed 3) (safety 0)))
    (loop for parent1-gene in (second parent1)
       for parent2-gene in (second parent2)
       do (setf prob (random 999))
       if (< prob 490)
       collect parent1-gene into offspring
       if (and (>= prob 490) (< prob 980))
       collect parent2-gene into offspring
       if (>= prob 980)
       collect (mutation) into offspring
       finally(return (inversion offspring prob)))))


;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence ()
  (declare (optimize (speed 3) (safety 0)))
  (loop for i from 1 to *board*
     collect (nth (random (length *colors*)) *colors*) into generated-candidate
     finally (return (list 0 generated-candidate))))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-top-fifty-candidate (population)
  (declare (optimize (speed 3) (safety 0)))
  (nth (random (floor *50-percent-of-size*)) population))


;; Helper function for play-candidate-with-guess
;; Credit for original "spot" function goes to Professor Susan Epstein
(defun spot-color (color)
  (case color
    (A 0)
    (B 1)
    (C 2)
    (D 3)
    (E 4)
    (F 5)
    (G 6)
    (H 7)
    (I 8)
    (J 9)
    (K 10)
    (L 11)
    (M 12)
    (N 13)
    (O 14)
    (P 15)
    (Q 16)
    (R 17)
    (S 18)
    (TT 19)
    (U 20)
    (V 21)
    (W 22)
    (X 23)
    (Y 24)
    (Z 25)))

;; Helper function for process-candidate-with-guess.
;; Original "color-counter" credit goes to Professor Susan Epstein
(defun count-color (list)
  (loop with tally = (make-array *number-of-colors-initial* :initial-element 0)
     for peg in list
     for index = (spot-color peg)
     do (incf (aref tally index))
     finally (return tally)))


;; Function plays two different guesses against each other and returns
;; the pegs with exact color and position (black pegs), and pegs with color but not
;; position (white pegs)
;; Credit for original "process-guess" function goes to Professor Susan Epstein
(defun process-candidate-with-guess (candidate guess)
  (loop with answer = candidate
     with guess-color-count = (count-color guess)
     with true-color-count = (count-color answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot-color entry)))
     and do (decf (aref true-color-count (spot-color entry)))
     finally (progn
	       (return (list exact-counter (loop for i from 0 to (1- (length *colors*))
					      for guessed = (aref true-color-count i)
					      for true = (aref guess-color-count i)
					      when (<= true guessed)
					      sum true
					      else sum guessed))))))

;; Calculate difference of black pegs of candidate c with previous guesses
(defun summate-black-peg-difference (candidate)
  (declare (optimize (speed 3) (safety 0)))
  (loop for guess in *guesses*
     sum (abs (- (first (process-candidate-with-guess candidate (third guess)))
		 (first guess)))))

;; Calculate difference of white pegs of candidate c with previous guesses
(defun summate-white-peg-difference (candidate)
  (declare (optimize (speed 3) (safety 0)))
  (loop for guess in *guesses*
     sum (abs (- (second (process-candidate-with-guess candidate (third guess)))
		 (second guess)))))

;; Calculate fitness by heuristic formula described in comment header
(defun calculate-fitness (candidate)
  (declare (optimize (speed 3) (safety 0)))
  (+ (* *weight-a* (summate-black-peg-difference candidate))
     (summate-white-peg-difference candidate)
     (* *weight-b* *board* (1- *turns-played*))))

;; NOT USED
;; ;; Calculate fitness by heuristic, described in comment header
;; (defun calculate-similarity (candidate population)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (let ((population-without-candidate (remove candidate population :key #'second))
;; 	score)
;;   (loop for guess in population-without-candidate
;;        do (setf score (process-candidate-with-guess (second candidate) (second guess)))
;;       sum (+ (first score) (second score)))))


;; Return list with elite 10% of population
(defun get-elite-10-percent (population)
  (declare (optimize (speed 3) (safety 0)))
  (let (elite-population counter)
    (setf counter 1)
    (loop until (= (length elite-population) *10-percent-of-size*)
       when (and (not (guessed-alreadyp (nth counter population)))
		 (not (member (second (nth counter population)) elite-population
			      :test #'equal :key #'second)))
       do (setf elite-population (append elite-population (list (nth counter population))))
       do (setf counter (1+ counter))
       finally (return elite-population))))

;; Return list with mated top 50% to form remaining 90% of population
(defun get-mated-90-percent (population)
  (declare (optimize (speed 3) (safety 0)))

  (let (offspring mated-population)
    (loop until (= (length mated-population) *90-percent-of-size*)
       do (setf offspring (mate (random-top-fifty-candidate population)
				(random-top-fifty-candidate population)))
       when (and (not (guessed-alreadyp offspring))
		 (not (member offspring mated-population :test #'equal :key #'second)))
       do (setf mated-population (append mated-population (list (list 0 offspring))))
       finally (return mated-population))))

;; Assign fitness values to population
(defun return-population-with-fitness (population)
  (declare (optimize (speed 3) (safety 0)))
  (loop for candidate in population
     collect (list (calculate-fitness (second candidate)) (second candidate))))

;; Generate new populations using elitism and mating until reaching max-generations
;; Comb each generation for duplicates and present in previous generation
(defun generation-loop (population)
  (declare (optimize (speed 3) (safety 0)))
  (let ((generation population)
	old-generation)
    (loop for i from 1 to *max-generations*
       do (setf old-generation generation)
       do (setf generation (get-elite-10-percent generation))
       do (setf generation (append generation (get-mated-90-percent old-generation)))
       do (setf generation (return-population-with-fitness generation))
       do (setf generation (sort generation #'< :key #'fitness))
       finally (return generation))))

;; NOT USED
;; Choose best guess from new-population (sceondary heuristic),
;; plays each a candidate against all others and chooses the candidate
;; that scores the highest (most similar)
;; (defun choose-best-guess (population)
;;   (declare (optimize (speed 3) (safety 0)))
;;   (let ((similarity 0)
;; 	(highest-similarity 0)
;; 	best-guess)
;;     ;(format t "~%~%Similarity heuristic:")
;;     (loop for candidate in population
;;        do (setf similarity (calculate-similarity candidate population))
;;        do (format t "~%~a : ~a" (second candidate) similarity)
;;        when (> similarity highest-similarity)
;;        do (setf highest-similarity similarity)
;;        and do (setf best-guess (second candidate))
;;        finally (return (list best-guess)))))

;; Remove guessed
(defun guessed-alreadyp (candidate)
  (member (second candidate) *guesses* :test #'equal :key #'third))

;; Remove duplicates
(defun remove-duplicate-candidates (population)
  (remove-duplicates population :test #'equal :key #'second))


;;------------------------------------------------------
;; MAIN ROUTINE
;;------------------------------------------------------
(defun MoonlightPinkFlamingoes (board colors SCSA last-response)
  (let (guess)
    ;; First turn
    (cond ((null last-response) 
	   (progn
	     ;; Clear previously saved values
	     ;; Initialize and clear main variables
	     (setf *previous-population* nil)
	     (setf *guesses* nil)
	     (setf *max-size* 60) ;; Population size, default = 60
	     (setf *10-percent-of-size* (* 10 (/ *max-size* 100)))
	     (setf *90-percent-of-size* (* 90 (/ *max-size* 100))) 
	     (setf *50-percent-of-size* (* 50 (/ *max-size* 100)))
	     (setf *max-generations* 100);; Max generations, default = 300
	     (setf *population-size* 150)
	     (setf *colors* colors)
	     (setf *board* board)
	     (setf *weight-a* 1)
	     (setf *weight-b* 2)
	     (setf *turns-played* 0)
	     (setf *SCSA-constraints* t)
	     (setf *number-of-colors-initial* (+ (length *colors*) 1))

	     ;; Adjust max population size and generations to avoid excessively looping
	     ;; when not necessary in higher peg/color combos
	     (cond ((>= 12 *board*) (progn (setf *max-size* 30) (setf *max-generations* 40)))
		   ((>= 10 *board*) (progn (setf *max-size* 40) (setf *max-generations* 50)))
		   (t nil))

	     ;; SCSA initialization cond table
	     (cond
	       ;; SCSA: AB-COLOR
	       ;; The domain for ab-color is already known. Set colors to (A B)
	       ((equal SCSA 'ab-color)
		(progn
		  (setf *colors* '(A B))
		  (setf *SCSA-constraints* nil)
		  (setf *previous-population* (initialize-population))
		  (setf guess (second (create-gene-sequence)))))

	       ;; SCSA: TWO-COLOR
	       ;; Initial guess for two-color SCSA is a solid color guess using the first
	       ;; color in *colors*
	       ;; Most effective (does not inflate total guess count unneccesarily) in higher
	       ;;   peg/color combos
	       ((and (equal SCSA 'two-color) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))

	       ;; SCSA: TWO-COLOR-ALTERNATING
	       ;; Initial guess for two-color-alternating SCSA is a solid color guess using the first
	       ;; color in *colors*
	       ;; Most effective in higher peg/color combos
	       ((and (equal SCSA 'two-color-alternating) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))
	       
	       ;; SCSA: USUALLY-FEWER
	       ;; Initial guess for usually-fewer is a solid color guess using first color in *colors*
	       ;; Most effective in higher peg/color combos
	       ((and (equal SCSA 'usually-fewer) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))

	       ;; SCSA: PREFER-FEWER
	       ;; Initial guess for usually-fewer is a solid color guess using first color in *colors*
	       ;; Most effective in higher peg/color combo
	       ((and (equal SCSA 'prefer-fewer) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))

	       ;; SCSA: MYSTERY-1
	       ;; OBSERVABLE: Somewhat randomized SCSA selection
	       ;;((and ()) ())
	       
	       ;; SCSA: MYSTERY-2
	       ;; OBSERVABLE: Three colors alternating
	       ;; Make initial guesses to limit the domain to three colors. Start with first color
	       ;; in *colors* and make a solid color guess
	       ((and (equal SCSA 'mystery-2) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))
	       
	       ;; SCSA: MYSTERY-3
	       ;; OBSERVABLE: Three colors
	       ;; Make initial guesses to limit the domain to three colors. Start with first color
	       ;; in *colors* and use that to make first solid color guess.
	       ((and (equal SCSA 'mystery-3) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))
	       
	       ;; SCSA: MYSTERY-4
	       ;; OBSERVABLE: Four colors
	       ;; Make initial guesses to limit the domain to four colors. Start with first color
	       ;; in *colors* and make a solid color guess.
	       ((and (equal SCSA 'mystery-4) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))
	       
	       ;; SCSA: MYSTERY-5
	       ;; OBSERVABLE: two-color-alternating
	       ((and (equal SCSA 'mystery-5) (>= *board* 12) (>= (length *colors*) 14))
		(setf guess (make-list *board* :initial-element 'A)))
	       
	       ;; If SCSA is has not implementation/not needed for board/size combo, return rando
	       (t (progn
		    (setf guess (second (create-gene-sequence)))
		    (setf *previous-population* (initialize-population))
		    (setf *SCSA-constraints* nil))))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; After SCSA has it's first guess, send it
	     guess))

	  ;;---------------------------------------------------------------
	  ;; SCSA CONSTRAINTS APPLICATION
	  ;;---------------------------------------------------------------
	  
	  ;; SCSA: TWO-COLOR
	  ;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
	  ;;              Keep removing until only two colors left.
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'two-color)
		(not (eq (length *colors*) 2)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))

	  ;; SCSA: TWO-COLOR-ALTERNATING
	  ;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
	  ;;              Keep removing until only two colors left.
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'two-color-alternating)
		(not (eq (length *colors*) 2)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))

	  
	  ;; SCSA: USUALLY-FEWER
	  ;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
	  ;;              Keep removing until 3 colors left. Although SCSA has possibility to generate
	  ;;              2 colors, the complexity and guesses used raises to pinpoint exact number,
	  ;;              therefore settle with
	  ;;              restricted domain of 3 colors.
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'usually-fewer)
		(not (eq (length *colors*) 3)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test

		   ;; Move color to back
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;; (print *colors*)

	     ;; Construct next guess
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record next guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))

	  ;; SCSA: PREFER-FEWER
	  ;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
	  ;;              Keep removing until 3 colors left. Although SCSA has possibility to generate
	  ;;              5 or less, the complexity and guesses used raises to pinpoint exact number,
	  ;;              therefore settle with 5 or make random guesses until 100 guess limit is
	  ;;              reached (no auto-disqualify for guess limit)
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'prefer-fewer)
		(not (eq (length *colors*) 5)))
	   (progn
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Move to back
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))
	     ;; DEBUG
	     ;; (print *colors*)

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)

	     ;; Send guess
	     guess))

	  ;; SCSA: MYSTERY-1 (too randomized, may be less complex for genetic to try unaided)
	  ;; Constraints: nil

	  
	  ;; SCSA: MYSTERY-2
	  ;; Constraints: Since the observable pattern is three colors alternating,
	  ;;              limit the domain (colors) to 3
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'mystery-2)
		(not (eq (length *colors*) 3)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))


	  ;; SCSA: MYSTERY-3
	  ;; Constraints: Since the observable pattern is three colors.
	  ;;              limit the domain (colors) to 3
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'mystery-3)
		(not (eq (length *colors*) 3)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))


	  ;; SCSA: MYSTERY-4
	  ;; Constraints: Since the observable pattern is 4 colors,
	  ;;              limit the domain (colors) to 4
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'mystery-4)
		(not (eq (length *colors*) 3)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))


	  ;; SCSA: MYSTERY-5
	  ;; Constraints: Since the observable pattern is two colors alternating,
	  ;;              limit the domain (colors) to 2
	  ((and (>= *board* 12)
		(>= (length *colors*) 14)
		(equal SCSA 'mystery-5)
		(not (eq (length *colors*) 3)))
	   (progn
	     ;; If last response was a total of 0, color not present in answer, therefore remove
	     (if (and (eq 0 (first last-response))
		      (eq 0 (second last-response)))
		 (progn
		   ;; Remove color
		   (setf *colors* (remove (first *colors*) *colors*)))
		 (progn
		   ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
		   ;; and prepare next color to test
		   ;; Move color to back of *colors*
		   (setf *colors* (append *colors* (list (first *colors*))))
		   (setf *colors* (remove (first *colors*) *colors* :count 1))))

	     ;; Retrieve score from previous guess
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))
	     
	     ;; DEBUG
	     ;;(print *colors*)

	     ;; Construct and send a solid color guess using current first element of *colors*
	     (setf guess (make-list *board* :initial-element (first *colors*)))

	     ;; Record guess
	     (push (list guess) *guesses*)
	     
	     ;; Send guess
	     guess))

	  
	  ;; After all SCSA conditions are satisfied, move on to genetic algorithm (general player)
	  (T
	   (progn
	     (let (new-population)
	       ;; If SCSA constraints were applied, ignore all previous guesses and start main algoritm
	       ;; here using genetic-algorithm
	       (if (equal *SCSA-constraints* 't)
		   (progn
		     ;; Retrieve score for previous score
		     ;; ... Push white pegs
		     (push (second last-response) (first *guesses*))
		     ;; ... Push black pegs
		     (push (first last-response) (first *guesses*))
		     
		     ;; Flag was received, set to nil to allow routine to proceed to genetic algorithm
		     (setf *SCSA-constraints* nil)
		     ;; Set guess to a randomly generated sequence using *colors* and *board*
		     (setf guess (second (create-gene-sequence)))
		     ;; Keep track of this as a guesss
		     (push (list guess) *guesses*)
		     ;; Prepare initial population for the algorithm
		     (setf *previous-population* (initialize-population))
		     ;; Send guess
		     guess)
		   (progn
		     ;; DEBUG
		     ;;(print last-response)
		     ;;(format t "~%Score for above guess: ~a~%" last-response)

		     ;; iterate turn counter
		     (setf *turns-played* (1+ *turns-played*))
		     ;; Give last guess its result)
		     ;; ... Push white pegs
		     (push (second last-response) (first *guesses*))
		     ;; ... Push black pegs
		     (push (first last-response) (first *guesses*))


		     ;; Set new-population to the resulting population of generation-loop
		     ;; this population will have been mated the specified number of times
		     (setf new-population (generation-loop *previous-population*))

		     ;; Record this new-population for use in diversifying next turns populations
		     ;; Remove all duplicates and already guessed candidates, since they limit
		     ;;  genetic diversity
		     (setf *previous-population*
			   (remove-duplicate-candidates (remove-if #'guessed-alreadyp new-population)))

		     ;; Record new-population and remove already guessed candidates (functions
		     ;;  already duplicate check)
		     (setf new-population (remove-if #'guessed-alreadyp new-population))

		     ;; DEBUG
		     ;; (format t "~%New population:")
		     ;; (loop for i in new-population
		     ;;    do (print i))
		     
		     ;; Set guess to the most fit candidate in new-population
		     (setf guess (second (first new-population)))

		     ;; Push guess onto list of previous guesses
		     (push (list guess) *guesses*)

		     ;; Guess has been chosen, send it to get scored
		     guess))))))))
