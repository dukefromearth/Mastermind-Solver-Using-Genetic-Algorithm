
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

;; Used to store remaining colors that haven't been guessed in the "rapid-fire" algorithm
(defvar *rapid-fire-colors*)
(defvar *last-guess*)

;; Generate a population of specified size at random
(defun initialize-population ()
  (let (population candidate)
    (loop until (= (length population) *population-size*)
       do (setf candidate (create-gene-sequence))
       when (not (member (second candidate) population :test #'equal :key #'second))
       do (setf population (append population (list candidate)))
       and do (setf population (remove-duplicate-candidates population))
       finally (return population))))

;; Returns fitness value, does not calculate, this is used in sorting operations
(defun fitness (candidate)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list candidate))
  (first candidate))

;; Choose random gene from *colors* (gene pool)
(defun mutation ()
  ;; Compiler optimization
  (declare (optimize (speed 3) (safety 0)))
  ;; Select random gene from *colors* (gene pool)
  (nth (random (length *colors*)) *colors*))

;; Swaps two genes at random, low chance.
(defun inversion (offspring prob)
  ;; Compiler optimization and type delcaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list offspring))
  (declare (type integer prob))

  ;; Get two random positions within the offspring
  (let ((random-spot1 (random (length offspring)))
	(random-spot2 (random (length offspring))))
    ;;If number chosen at random above 990, swap the elements at random positions
    (if (>= prob 990)
	(setf (nth random-spot1 offspring) (nth random-spot2 offspring)))
    offspring))

;; Mate two guesses and produce an offspring using probability based crossover
(defun mate (parent1 parent2)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list parent1 parent2))
  (let (prob)
    ;; Loop through the genes of both parents in parallel
    (loop for parent1-gene in (second parent1)
       for parent2-gene in (second parent2)
       ;; Choose a random number
       do (setf prob (random 999))
       if (< prob 490)
       ;; Choose parent-1 gene
       collect parent1-gene into offspring
       if (and (>= prob 490) (< prob 980))
       ;; Choose parent-2 gene
       collect parent2-gene into offspring
       if (>= prob 980)
       collect (mutation) into offspring
       ;; Finally take mated offspring, and check if inversion will occur with the
       ;; given probability
       finally(return (inversion offspring prob)))))


;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence ()
  ;; Compiler optimization
  (declare (optimize (speed 3) (safety 0)))
  (loop for i from 1 to *board*
     ;; Loop through every possible position of the elements in candidate,
     ;; then randomly pick a gene from *colors* (gene pool)
     collect (nth (random (length *colors*)) *colors*) into generated-candidate
     finally (return (list 0 generated-candidate))))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-top-fifty-candidate (population)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))

  ;; Choose a random candidate from first 50% of the population
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
	       (return (list exact-counter (loop for i from 0 to (1- *number-of-colors-initial*)
					      for guessed = (aref true-color-count i)
					      for true = (aref guess-color-count i)
					      when (<= true guessed)
					      sum true
					      else sum guessed))))))

;; Calculate difference of black pegs of candidate c with previous guesses
(defun summate-black-peg-difference (candidate)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list candidate))

  ;; Loop through every previous guess and return the sum of the difference of black pegs.
  (loop for guess in *guesses*
     sum (abs (- (first (process-candidate-with-guess candidate (third guess)))
		 (first guess)))))

;; Calculate difference of white pegs of candidate c with previous guesses
(defun summate-white-peg-difference (candidate)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list candidate))
  
  ;; Loop through every previous guess and return the sum of the difference of white pegs.
  (loop for guess in *guesses*
     sum (abs (- (second (process-candidate-with-guess candidate (third guess)))
		 (second guess)))))

;; Calculate fitness by heuristic formula described in comment header
(defun calculate-fitness (candidate)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list candidate))

  ;; Calculate the fitness value using the fitness heuristic formula
  (+ (* *weight-a* (summate-black-peg-difference candidate))
     (summate-white-peg-difference candidate)
     (* *weight-b* *board* (1- *turns-played*))))


;; Return list with elite 10% of population
(defun get-elite-10-percent (population)
  ;; Compiler optimizations and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))

  ;; Loop through the population until 10% of max-size is allocated
  (let (elite-population counter)
    (setf counter 1)
    ;; Continue looping while ignoring duplicates and already guessed candidates
    (loop until (= (length elite-population) *10-percent-of-size*)
       when (and (not (guessed-alreadyp (nth counter population)))
		 (not (member (second (nth counter population)) elite-population
			      :test #'equal :key #'second)))
       do (setf elite-population (append elite-population (list (nth counter population))))
       do (setf counter (1+ counter))
       finally (return elite-population))))

;; Return list with mated top 50% to form remaining 90% of population
(defun get-mated-90-percent (population)
  ;; Compiler optimizations and type declarations
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))

  ;; Loop through the population, until 90% of the max-size is reached
  (let (offspring mated-population)
    ;; Continues looping while ignoring duplicates and already guessed candidates
    (loop until (= (length mated-population) *90-percent-of-size*)
       ;; Mate two random candidates in the top 50% of the population
       do (setf offspring (mate (random-top-fifty-candidate population)
				(random-top-fifty-candidate population)))
       when (and (not (guessed-alreadyp offspring))
		 (not (member offspring mated-population :test #'equal :key #'second)))
       do (setf mated-population (append mated-population (list (list 0 offspring))))
       finally (return mated-population))))

;; Assign fitness values to population
(defun return-population-with-fitness (population)
  ;; Compiler optimizations and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))

  ;; Calculate the fitness of every candidate and return the candite with its fitness
  ;; in a new list
  (loop for candidate in population
     collect (list (calculate-fitness (second candidate)) (second candidate))))

;; Generate new populations using elitism and mating until reaching max-generations
;; Comb each generation for duplicates and present in previous generation
(defun generation-loop (population)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))
  (let ((generation population)
	old-generation)
    ;; Continually regenerate the population until limit is reached
    (loop for i from 1 to *max-generations*
       do (setf old-generation generation)
       ;; Add the elite 10% unmodified to the new population
       do (setf generation (get-elite-10-percent generation))
       ;; Get the remaining 90% of mated candidates and add to new population
       do (setf generation (append generation (get-mated-90-percent old-generation)))
       ;; Retrieve the fitness values for every populant
       do (setf generation (return-population-with-fitness generation))
       ;; Sort the list in ascending order (first element has lowest fitness value)
       do (setf generation (sort generation #'< :key #'fitness))
       ;; Finally, return new, thoroughly mated, population
       finally (return generation))))

;; Remove guessed
(defun guessed-alreadyp (candidate)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list candidate))
  ;; Return true if candidate is present in *guesses*, set of all previous guesses
  (member (second candidate) *guesses* :test #'equal :key #'third))

;; Remove duplicates
(defun remove-duplicate-candidates (population)
  ;; Compiler optimization and type declaration
  (declare (optimize (speed 3) (safety 0)))
  (declare (type list population))
  ;; Remove duplicates that are identical guesses
  (remove-duplicates population :test #'equal :key #'second))

(defun record-pegs-into-last-guess (last-response)
        ;; Retrieve score from previous guess
      ;; ... Push white pegs
      (push (second last-response) (first *guesses*))
      ;; ... Push black pegs
      (push (first last-response) (first *guesses*)))

;; Used to guess all possible colors in the format (AAA), (BBB), (CCC)
(defun rapid-fire ()
  (let (guess)
    (progn
      (setf guess (make-list *board* :initial-element (first *rapid-fire-colors*)))
      (setf *rapid-fire-colors* (rest *rapid-fire-colors*))
      (setf *last-guess* guess))
    guess))

;;------------------------------------------------------
;; SCSA check + initialization of first guess
;;------------------------------------------------------

;; Check for SCSA and initialize to predetermined first guess, otherwise, use first guess function
(defun initialize-first-guess (*board* *colors* SCSA)
  (let (guess)
  ;; SCSA initialization cond table
    (cond
      ;; SCSA: AB-COLOR
      ;; The domain for ab-color is already known. Set colors to (A B)
      ((and (equal SCSA 'ab-color)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
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
      ((and (equal SCSA 'two-color)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: TWO-COLOR-ALTERNATING
      ;; Initial guess for two-color-alternating SCSA is a solid color guess using the first
      ;; color in *colors*
      ;; Most effective in higher peg/color combos
      ((and (equal SCSA 'two-color-alternating)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: USUALLY-FEWER
      ;; Initial guess for usually-fewer is a solid color guess using first color in *colors*
      ;; Most effective in higher peg/color combos
      ((and (equal SCSA 'usually-fewer)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: PREFER-FEWER
      ;; Initial guess for usually-fewer is a solid color guess using first color in *colors*
      ;; Most effective in higher peg/color combo
      ((and (equal SCSA 'prefer-fewer)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: MYSTERY-1
      ;; OBSERVABLE: Somewhat randomized SCSA selection
      ;;((and ()) ())
      
      ;; SCSA: MYSTERY-2
      ;; OBSERVABLE: Three colors alternating
      ;; Make initial guesses to limit the domain to three colors. Start with first color
      ;; in *colors* and make a solid color guess
      ((and (equal SCSA 'mystery-2)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: MYSTERY-3
      ;; OBSERVABLE: Three colors
      ;; Make initial guesses to limit the domain to three colors. Start with first color
      ;; in *colors* and use that to make first solid color guess.
      ((and (equal SCSA 'mystery-3)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: MYSTERY-4
      ;; OBSERVABLE: Four colors
      ;; Make initial guesses to limit the domain to four colors. Start with first color
      ;; in *colors* and make a solid color guess.
      ((and (equal SCSA 'mystery-4)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; SCSA: MYSTERY-5
      ;; OBSERVABLE: two-color-alternating
      ((and (equal SCSA 'mystery-5)
	    (>= *board* 12)
	    (>= *number-of-colors-initial* 14))
       (setf guess (make-list *board* :initial-element 'A)))
      
      ;; If SCSA is has not implementation/not needed for board/size combo, return rando
      (t (progn
	   (setf guess (second (create-gene-sequence)))
	   (setf *previous-population* (initialize-population))
	   (setf *SCSA-constraints* nil))))
    guess))

;;------------------------------------------------------
;; SCSA ALGORITHMS
;;------------------------------------------------------

;; SCSA: TWO-COLOR
;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
;;              Keep removing until only two colors left.
(defun run-two-color (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)
      ;; Construct and send a solid color guess using current first element of *colors*
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))

;; SCSA: TWO-COLOR-ALTERNATING
;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
;;              Keep removing until only two colors left.
(defun two-color-helper (colors)
  (loop for i from 1 to *board*
     collect (nth (rem i 2) colors)))

(defun run-two-color-alternating (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)

      (if (= 2 (length *colors*))
	  (progn
	    (setf guess (two-color-helper *colors*))
	    (setf *colors* (reverse *colors*)))
	  (setf guess (make-list board :initial-element (first *colors*))))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))

;; SCSA: USUALLY-FEWER
;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
;;              Keep removing until 3 colors left. Although SCSA has possibility to generate
;;              2 colors, the complexity and guesses used raises to pinpoint exact number,
;;              therefore settle with
;;              restricted domain of 3 colors.
(defun run-usually-fewer (board colors last-response)
  (let (guess)
    (progn
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first *colors*) *colors*)))
	  (progn
	    ;; Move to back
	    (setf *colors* (append colors (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)
      ;; Construct next guess
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)      
      ;; Send guess
      guess)))

;; SCSA: PREFER-FEWER
;; Constraints: If last solid color guess returns a (0 0) response, remove it from *colors*.
;;              Keep removing until 3 colors left. Although SCSA has possibility to generate
;;              5 or less, the complexity and guesses used raises to pinpoint exact number,
;;              therefore settle with 5 or make random guesses until 100 guess limit is
;;              reached (no auto-disqualify for guess limit)
(defun run-prefer-fewer (board colors last-response)
  (let (guess)
    (progn
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first *colors*) *colors*)))
	  (progn
	    ;; Move to back
	    (setf *colors* (append colors (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)
      ;; Construct and send a solid color guess using current first element of *colors*
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess))
    )


;; SCSA: MYSTERY-2
;; Constraints: Since the observable pattern is three colors alternating,
;;              limit the domain (colors) to 3
(defun run-mystery-2 (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      ;; Retrieve score from previous guesslement (first *colors*)))
      (record-pegs-into-last-guess last-response)
      ;; Construct and send a solid color guess using current first element of *colors*
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))

  	   
;; SCSA: MYSTERY-3
;; Constraints: Since the observable pattern is three colors.
;;              limit the domain (colors) to 3
(defun run-mystery-3 (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)
      ;; Construct and send a solid color guess using current first element of *colors*
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))

;; SCSA: MYSTERY-4
;; Constraints: Since the observable pattern is 4 colors,
;;              limit the domain (colors) to 4
(defun run-mystery-4 (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)
      ;; DEBUG
      ;;(print *colors*)
      ;; Construct and send a solid color guess using current first element of *colors*
      (setf guess (make-list board :initial-element (first *colors*)))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))


;; SCSA: MYSTERY-5
;; Constraints: Since the observable pattern is two colors alternating,
;;              limit the domain (colors) to 2
(defun run-mystery-5 (board colors last-response)
  (let (guess)
    (progn
      ;; If last response was a total of 0, color not present in answer, therefore remove
      (if (and (eq 0 (first last-response))
	       (eq 0 (second last-response)))
	  (progn
	    ;; Remove color
	    (setf *colors* (remove (first colors) *colors*)))
	  (progn
	    ;; Otherwise, color is present, move to back of *colors* to avoid deletion,
	    ;; and prepare next color to test
	    ;; Move color to back of *colors*
	    (setf *colors* (append *colors* (list (first *colors*))))
	    (setf *colors* (remove (first *colors*) *colors* :count 1))))
      (record-pegs-into-last-guess last-response)

      (if (= 2 (length *colors*))
	  (progn
	    (setf guess (two-color-helper *colors*))
	    (setf *colors* (reverse *colors*)))
	  (setf guess (make-list board :initial-element (first *colors*))))
      ;; Record guess
      (push (list guess) *guesses*)
      ;; Send guess
      guess)))



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
	     (setf *max-size* 60) 
	     (setf *10-percent-of-size* (* 10 (/ *max-size* 100)))
	     (setf *90-percent-of-size* (* 90 (/ *max-size* 100))) 
	     (setf *50-percent-of-size* (* 50 (/ *max-size* 100)))
	     (setf *max-generations* 100)
	     (setf *population-size* 150)
	     (setf *colors* colors)
	     (setf *board* board)
	     (setf *weight-a* 1)
	     (setf *weight-b* 2)
	     (setf *turns-played* 0)
	     (setf *SCSA-constraints* t)
	     (setf *number-of-colors-initial* (length *colors*))
	     (setf *rapid-fire-colors* colors)
	     ;; Adjust max population size and generations to avoid excessively looping
	     ;; when not necessary in higher peg/color combos
	     (cond
	       ((>= *board* 18) (progn (setf *max-size* 40) (setf *max-generations* 10) (setf *population-size* 30)))
	       ((>= *board* 15) (progn (setf *max-size* 60) (setf *max-generations* 30) (setf *population-size* 40)))
	       ((>= *board* 12) (progn (setf *max-size* 30) (setf *max-generations* 40) (setf *population-size* 50)))
	       ((>= *board* 10) (progn (setf *max-size* 40) (setf *max-generations* 50) (setf *population-size* 50)))
	       (t nil))
	     (setf guess (initialize-first-guess board colors SCSA))
	     ;; Record guess
	     (push (list guess) *guesses*)
	     ;; After SCSA has it's first guess, send it
	     guess))
	  
	  ;;---------------------------------------------------------------
	  ;; SCSA CONSTRAINT APPLICATION
	  ;;---------------------------------------------------------------

	  ;; SCSA: TWO-COLOR
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'two-color)
		(not (eq (length *colors*) 2)))
	   (run-two-color *board* *colors* last-response))

	  ;; SCSA: TWO-COLOR-ALTERNATING
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'two-color-alternating)
		(not (eq (length *colors*) 2)))
	   (run-two-color-alternating *board* *colors* last-response))
	  
	  ;; SCSA: USUALLY-FEWER
	  ((and (>= *board* 12)
		(>=  *number-of-colors-initial* 14)
		(equal SCSA 'usually-fewer)
		(not (eq (length *colors*) 3)))
	   (run-usually-fewer *board* *colors* last-response))

	  ;; SCSA: PREFER-FEWER
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'prefer-fewer)
		(not (eq (length *colors*) 5)))
	   (run-prefer-fewer *board* *colors* last-response))	   

	  ;; SCSA: MYSTERY-1 (too randomized, may be less complex for genetic to try unaided)
	  ;; Constraints: nil
	  
	  ;; SCSA: MYSTERY-2
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'mystery-2)
		(not (eq (length *colors*) 3)))
	   (run-mystery-2 *board* *colors* last-response))

	  ;; SCSA: MYSTERY-3
	  ((and (>= *board* 12)
		(>=  *number-of-colors-initial* 14)
		(equal SCSA 'mystery-3)
		(not (eq (length *colors*) 3)))
	   (run-mystery-3 *board* *colors* last-response))

	  ;; SCSA: MYSTERY-4
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'mystery-4)
		(not (eq (length *colors*) 4)))
	   (run-mystery-4 *board* *colors* last-response))

	  ;; SCSA: MYSTERY-5
	  ((and (>= *board* 12)
		(>= *number-of-colors-initial* 14)
		(equal SCSA 'mystery-5)
		(not (eq (length *colors*) 2)))
	   (run-mystery-5 *board* *colors* last-response))

	  ;; Guess all colors
	  ((and (> (+ board (length colors)) 25) *rapid-fire-colors*)
	   (if (= 0 (+ (first last-response) (second last-response)))
	       (setf *colors* (remove (first *last-guess*) *colors* :test #'eq)))
	   (rapid-fire))
	   
	  
	  ;; After all SCSA conditions are satisfied, move on to genetic algorithm (general player)
	  (T
	   (progn
	     (let (new-population)
	       ;; If SCSA constraints were applied, ignore all previous guesses and start main algoritm
	       ;; here using genetic-algorithm
	       (if (equal *SCSA-constraints* 't)
		   (progn
		     (record-pegs-into-last-guess last-response)
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
		   ;; If SCSA contstraints already applied or nut used, preceed to genetic algorithm
		   (progn
		     (setf *turns-played* (1+ *turns-played*))
		     (record-pegs-into-last-guess last-response)
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
		     ;; Set guess to the most fit candidate in new-population
		     (setf guess (second (first new-population)))
		     ;; Push guess onto list of previous guesses
		     (push (list guess) *guesses*)
		     ;; Guess has been chosen, send it to get scored
		     guess))))))))
