;; Team name:    Moonlight Pink Flamingoes
;; Date created: April 8th, 2019
;; Description: Genetic algorithm implementation for Mastermind based on the paper "Efficient solutions for Mastermind using genetic algorithms" by Berghman, Goossens, and Leus.


;;;; Symbol list
;; Pegs       -> pegs
;; Xi         -> bulls / black pegs
;; Yi         -> cows / white pegs
;; Ei         -> population at cycle i
;; (E hat i ) -> new population E at cycle i
;; c          -> candidate
;; h          -> generation counter
;; maxgen     -> max loops/generation cycles
;; maxsize    -> max size of population

;;;; Psuedo code from paper
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

;;;; Format
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

(defvar *guesses*)
(defvar *max-generations*)
(defvar *max-size*)
(defvar *previous-population*)
(defvar *population-size*)
(defvar *colors*)
(defvar *board*)
(defvar *weight-a*)
(defvar *weight-b*)
(defvar *turns-played*)
(defvar *10-percent-of-size*)
(defvar *90-percent-of-size*)
(defvar *50-percent-of-size*)
(defvar *elite-percent*)
(defvar *eligible-set*)
(defvar *last-guess*)

;; Generate a population of specified size at random
(defun initialize-population ()
  (let (population candidate)
    (loop until (= (length population) *population-size*)
       do (setf candidate (create-gene-sequence))
       when (not (member candidate population :test #'equal))
       do (setf population (append population (list candidate)))
       and do (setf population (remove-duplicate-candidates population))
       finally (return population))))

;; Returns fitness value, does not calculate, this is used in sort operations
(defun fitness (candidate)
  (first candidate))



;; ************************************************************************
;; GENE MANIPULATION
;; ************************************************************************



;; Choose random gene
(defun mutate (child)
  (setf (nth (random (length child)) child) (nth (random (length *colors*)) *colors*))
  child)

;; Low chance of invervsion, swaps two genes at random
(defun inversion (offspring prob)
  (let ((random-spot1 (random (length offspring)))
	(random-spot2 (random (length offspring))))
    (if (>= prob 990)
	(setf (nth random-spot1 offspring) (nth random-spot2 offspring)))
	offspring))

;; Mate two guesses and produce an offspring using crossover

;; Splices the parent genes at 1 index
(defun splice (mom dad)
  (let (index)
    (setf index (random (length mom)))
    (append (subseq mom 0 index) (subseq dad index (length dad)))))

;; Splices the parent genes at 1 indexes
(defun bi-splice (mom dad)
  (let ((index1 (random (length mom))) (index2 (random (length dad))))
    (loop until (not (= index1 index2))
	 do (setf index1 (random (length mom)))
	 do (setf index2 (random (length dad))))
    (if (< index2 index1)
	(let (temp)
	  (setf temp index1)
	  (setf index1 index1)
	  (setf index2 temp)))
    (append (subseq mom 0 index1)
	    (subseq dad index1 index2)
	    (subseq mom index2 (length mom)))))

;; Mates two genies by splicing
(defun mate (mom dad)
  (let (mated-gene)
    (if (>= 0.5 (random 1.0))
	(setf mated-gene (splice mom dad))
	(setf mated-gene (bi-splice  mom dad)))
    (if (< (random 100) 4)
	(setf mated-gene (mutate mated-gene)))	
    mated-gene))

;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence ()
  (loop for i from 1 to *board*
     collect (nth (random (length *colors*)) *colors*)))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-x-from-list (x population)
  (loop for candidate from 1 to x
       collect (nth (random (floor (/ (length population) 2))) population)))


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

;; Helper function for play-candidate-with-guess
;; Original "color-counter" credit goes to Professor Susan Epstein
(defun count-color (list)
  (loop with tally = (make-array (length *colors*) :initial-element 0)
     for peg in list
     for index = (spot-color peg)
     do (incf (aref tally index))
     finally (return tally)))


;; Testing

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
	       ;(print answer)
	       (return (list exact-counter (loop for i from 0 to (1- (length *colors*))
					      for guessed = (aref true-color-count i)
					      for true = (aref guess-color-count i)
					      when (<= true guessed)
					      sum true
					      else sum guessed))))))




;; Calculate fitness by heuristic, described in comment header
(defun calculate-similarity (candidate population)
  (let ((population-without-candidate (remove candidate population :test #'equal))
	score)
  (loop for guess in population-without-candidate
     do (setf score (process-candidate-with-guess candidate guess))
     sum (+ (first score) (second score)))))


;; Return list with elite 10% of population
(defun get-elite-population (population)
  (let (elite-population counter)
    (setf counter 0)
    (loop until (>= (length elite-population) (* *elite-percent* (length population)))
       when (not (guessed-alreadyp (nth counter population)))
       do (setf elite-population (append elite-population (list (nth counter population))))
       do (setf counter (1+ counter))
       finally (return elite-population))))

;; Return list with mated top 50% to form remaining 90% of population
(defun get-mated-non-elite-population (population)
  (let (offspring mated-population)
    (loop until (>= (length mated-population) (* (- 1 *elite-percent*) (length population)))
       do (setf offspring (mate
			   (nth (random (length population)) population)
			   (nth (random (length population)) population)))
       when (and (not (guessed-alreadyp offspring))
		 (not (member offspring mated-population :test #'equal)))
       do (setf mated-population (append mated-population (list offspring)))
       finally (return mated-population))))


;; ************************************************************************
;; GENE MANIPULATION
;; ************************************************************************



;; Calculate difference of black pegs of candidate c with previous guesses
(defun summate-black-peg-difference (candidate)
  (loop for guess in *guesses*
     sum (abs (- (first (process-candidate-with-guess candidate (third guess)))
		 (first guess)))))

;; Calculate difference of white pegs of candidate c with previous guesses
(defun summate-white-peg-difference (candidate)
  (loop for guess in *guesses*
     sum (abs (- (second (process-candidate-with-guess candidate (third guess)))
		 (second guess)))))

(defun compare-fitness (a b)
  (< (first a) (first b)))

;; Calculate fitness by heuristic, described in comment header
(defun calculate-fitness (candidate)
    (+ (* *weight-a* (summate-black-peg-difference candidate))
       (summate-white-peg-difference candidate)
       (* *weight-b* *board* (1- *turns-played*))))

;; Assign fitness values to population
(defun return-population-with-fitness (population)
  (let (population-with-fitness)
    ;(break)
    (setf population-with-fitness
	  (loop for candidate in population
	     collect (list (calculate-fitness candidate) candidate)))
    (setf population-with-fitness (sort population-with-fitness #'< :key #'first))
    population-with-fitness))

(defun return-population-without-fitness (population)
  (loop for candidate in population
     collect (second candidate)))

(defun is-eligible-guess (guesses this-guess)
  (let (new-response (count 0) last-response)
    (loop for guess in guesses
	 do (progn
	      (setf new-response (process-candidate-with-guess (third guess) this-guess))
	      (setf new-response (cons (first new-response) (list (second new-response))))
	      (setf last-response (cons (first guess) (list (second guess))))
	      (if (not (and
			(= 0 (- (first new-response) (first last-response)))
			(= 0 (- (second new-response) (second last-response)))))
		  (setf count (1+ count))))
       finally (return (if (= count 0) T nil)))))
			      
(defun get-eligible-set (population)
  (let ((eligible nil))
    (loop for c in population
       do (if (is-eligible-guess *guesses* c)
	      (push c eligible))
       finally (return eligible))))

(defun new-gen-loop (population)
  (setf *eligible-set* nil)
  (let (child (count 0) new-population)
    (setf new-population (return-population-with-fitness population))
    (setf new-population (return-population-without-fitness new-population))
    (setf new-population (get-elite-population new-population))
    (setf new-population (append new-population
				 (loop until (= (length new-population) *population-size*)
				    do (setf child (mate
						    (nth (random (length population)) population)
						    (nth (random (length population)) population)))
				    do (if (is-eligible-guess *guesses* child)
					   (push child *eligible-set*))
				    do (push child new-population)
				    do (setf count (1+ count))
				    collect child)))))
	 

;; Choose best guess from new-population (sceondary heuristic),
;; plays each a candidate against all others and chooses the candidate
;; that scores the highest (most similar)
(defun choose-best-guess (population)
  (let ((similarity 0)
	(highest-similarity 0)
	best-guess)
    ;(format t "~%~%Similarity heuristic:")
    (loop for candidate in population
       do (setf similarity (calculate-similarity candidate population))
       ;do (format t "~%~a : ~a" (second candidate) similarity)
       when (> similarity highest-similarity)
       do (setf highest-similarity similarity)
       and do (setf best-guess candidate)
       finally (return (list best-guess)))))

;; Remove guessed
(defun guessed-alreadyp (candidate) (member candidate *guesses* :test #'equal :key #'third))

;; Remove duplicates
(defun remove-duplicate-candidates (population) (remove-duplicates population :test #'equal))

(defun random-guess ()
  (loop for i from 1 to *board*
       collect (nth (random *board*) *colors*)))

;; Main routine
(defun MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (cond ((null last-response) ;; First turn routine
	 (progn
	   (let (guess)
	     ;; Clear previously saved values
	     ;; Initialize and clear main variables
	     (setf *previous-population* nil)
	     (setf *guesses* nil)
	     (setf *max-size* 60)
	     (setf *10-percent-of-size* (* 10 (/ *max-size* 100)))
	     (setf *90-percent-of-size* (* 90 (/ *max-size* 100))) 
	     (setf *50-percent-of-size* (* 50 (/ *max-size* 100)))
	     (setf *max-generations* 150)
	     (setf *max-size* 60)
	     (setf *population-size* 150)
	     (setf *colors* colors)
	     (setf *board* board)
	     (setf *weight-a* 1)
	     (setf *weight-b* 2)
	     (setf *turns-played* 0)
	     (setf *elite-percent* 0.1)
	     
	     ;; Get the fitness from last-response, place it at (FITNESS (guess))
	     (if (and (= board 4) (= (length colors) 6))
		 (setf guess '(A A B C))
		 (setf guess (create-gene-sequence)))
	     ;(break)
	      ;; For board = 4, color = 6
	     (push (list guess) *guesses*)
	     ;; (print *guesses*)

	     ;; Since first turn, prepare variable for next routine
	     (setf *previous-population* (initialize-population))
	     ;(format t "~%INITIAL GUESS: ~a~%" guess)
	     ;; Play guess, only element in list at this point: ((guess))

	     (setf *last-guess* (first (first *guesses*)))
	     
	     (first (first *guesses*)))))
	(T
	 (progn
	   (let (new-population); best-guess)
	     (format nil "Last guess ~a" (first (first *guesses*)))
	     
	     (setf *turns-played* (1+ *turns-played*))
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))

	     (print *last-guess*)
	     (print last-response)
	     ;(print *eligible-set*)
	     ;(break)
	     ;; (setf new-population (generation-loop *last-guess* last-response (subseq *previous-population* 0 *population-size*)))

	     ;; ;(print "New-pop")
	     ;; ;(format t "~{~{~A ~}~%~}" (subseq new-population 0 20))
	     
	     (setf new-population *previous-population*)

	     ;; (setf new-population (remove-if #'guessed-alreadyp new-population))
	     (let ((count 0))
	       (loop until (or (>= count *max-generations*) (>= (length *eligible-set*) *max-size*))
		  do (setf new-population (new-gen-loop new-population))
		  do (setf count (1+ count))))

	     (setf *eligible-set* (remove-duplicates *eligible-set* :test #'equal))
	     
	     (cond
	       ((< (length *eligible-set*) 2)
		(progn
		  (print "eligible")
		  (push (list (first new-population)) *guesses*)))
	       (T
		(progn
		(print "elite")
		(push (choose-best-guess *eligible-set*) *guesses*))))
	     

	     (setf *last-guess* (first (first *guesses*)))
	     (print *last-guess*)
	     ;(break)
	     
	     (setf *previous-population* (remove-duplicates new-population :test #'equal))

		 
	     ;; (print "last guess")
	     ;; (print *last-guess*)
	     
	     
	     ;; Play guess at top of pile (the most elite)
	     (first (first *guesses*)))))))
