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

;; Generate a population of specified size at random
(defun initialize-population ()
  (loop for i from 1 to *population-size*
     collect (create-gene-sequence)))

;; Returns fitness value, does not calculate, this is used in sort operations
(defun fitness (candidate)
  (first candidate))

;; Choose random gene
(defun mutation ()
    (nth (random (length *colors*)) *colors*))
       
;; Mate two guesses and produce an offspring using crossover
(defun mate (parent1 parent2)
  (let (prob)
    (loop for parent1-gene in (second parent1)
       for parent2-gene in (second parent2)
       do (setf prob (random 99))
       if (< prob 45)
       collect parent1-gene
       if (and (>= prob 45) (< prob 90))
       collect parent2-gene
       if (>= prob 90)
       collect (mutation))))
  

;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence ()
  (loop for i from 1 to *board*
     collect (nth (random (length *colors*)) *colors*) into generated-candidate
     finally (return (list 0 generated-candidate))))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-top-fifty-candidate (population)
  (nth (random (floor (/ *population-size* 2))) population))


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


;; Assess candidate score playing against guess
;; Returns candidate score in this format: (blackpegs whitepegs)
;; Modfied function based on "process-guess" method by Professor Susan Epstein
(defun process-candidate-with-guess (candidate guess)
  (loop with answer = guess
     with guess-color-count = (count-color candidate)
     with true-color-count = (count-color answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot-color entry)))
     and do (decf (aref true-color-count (spot-color entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (length *colors*))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed)))))

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

;; Calculate fitness by heuristic, described in comment header
(defun calculate-fitness (candidate)
  (+ (* *weight-a* (summate-black-peg-difference candidate))
     (summate-white-peg-difference candidate)
     (* *weight-b* *board* (1- *turns-played*))))

;; Calculate fitness by heuristic, described in comment header
(defun calculate-similarity (candidate population)
  (let ((population-without-candidate (remove candidate population :key #'second))
	score)
  (loop for guess in population-without-candidate
       do (setf score (process-candidate-with-guess (second candidate) (second guess)))
      sum (+ (first score) (second score)))))


;; Return list with elite 10% of population
(defun get-elite-10-percent (population)
  (loop for i from 1 to (float *10-percent-of-size*)
     collect (nth i population)))

;; Return list with mated top 50% to form remaining 90% of population
(defun get-mated-90-percent (population)
  (let (offspring)
    (loop for i from 1 to (float *90-percent-of-size*)
       do (setf offspring (mate (random-top-fifty-candidate population)
				(random-top-fifty-candidate population)))
       append (list (list 0 offspring)))))
  
;; Assign fitness values to population
(defun return-population-with-fitness (population)
  (loop for candidate in population
     collect (list (calculate-fitness (second candidate)) (second candidate))))

;; Generate new populations using elitism and mating until reaching max-generations
;; Comb each generation for duplicates and present in previous generation
(defun generation-loop (population)
  (let ((generation population)
	old-generation)
    (loop for i from 1 to *max-generations*
       do (setf old-generation generation)
       do (setf *10-percent-of-size* (* 10 (/ (length generation) 100)))
       do (setf *90-percent-of-size* (* 90 (/ (length generation) 100)))
       do (setf *50-percent-of-size* (/ (length generation) 2))
       do (setf generation (get-elite-10-percent generation))
       do (setf generation (append generation (get-mated-90-percent old-generation)))
       do (setf generation (return-population-with-fitness generation))
       do (setf generation (sort generation #'< :key #'fitness))
       finally (return generation))))

;; Choose best guess from new-population (sceondary heuristic),
;; plays each a candidate against all others and chooses the candidate
;; that scores the highest (most similar)
(defun choose-best-guess (population)
  (let ((similarity 0)
	(highest-similarity 0)
	best-guess)
    (format t "~%~%Similarity heuristic:")
    (loop for candidate in population
       do (setf similarity (calculate-similarity candidate population))
       do (format t "~%~a : ~a" (second candidate) similarity)
       when (> similarity highest-similarity)
       do (setf highest-similarity similarity)
       and do (setf best-guess (second candidate))
       finally (return (list best-guess)))))

;; Remove guessed
(defun guessed-alreadyp (candidate)
  (member (second candidate) *guesses* :test #'equal :key #'third))


;; Remove duplicates
(defun remove-duplicate-candidates (population)
  (remove-duplicates population :test #'equal :key #'second))

;; Main routine
(defun genetic-agent (board colors SCSA last-response)
  (declare (ignore SCSA))

  (cond ((null last-response) ;; First turn routine
	 (progn
	   (let (guess)
	     ;; Clear previously saved values
	     ;; Initialize and clear main variables
	     (setf *previous-population* nil)
	     (setf *guesses* nil)
	     (setf *10-percent-of-size* nil)
	     (setf *90-percent-of-size* nil)
	     (setf *50-percent-of-size* nil)
	     (setf *max-generations* 100)
	     (setf *population-size* 150)
	     (setf *colors* colors)
	     (setf *board* board)
	     (setf *weight-a* 1)
	     (setf *weight-b* 1)
	     (setf *turns-played* 0)
	     
	     ;; Get the fitness from last-response, place it at (FITNESS (guess))
	     (if (and (= board 4) (= (length colors) 6))
		 (setf guess '(A A B C))
		 (setf guess (second (create-gene-sequence))))
	      ;; For board = 4, color = 6
	     (push (list guess) *guesses*)
	     ;; (print *guesses*)

	     ;; Since first turn, prepare variable for next routine
	     (setf *previous-population* (initialize-population))
	     (format t "~%INITIAL GUESS: ~a~%" guess)
	     ;; Play guess, only element in list at this point: ((guess))
	     (first (first *guesses*)))))
	(T
	 (progn
	   (let (new-population best-guess)
	    ; (print (first (first *guesses*)))
	    ; (print last-response)
	     ;; iterate turn counter
	     (setf *turns-played* (1+ *turns-played*))
	     (format t "Score for above guess: ~a~%" last-response)
	     ;; Give last guess its result)
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))

	     ;; Extra info: Previous guess
	     ;; (print "")
	     ;; (print "Last guess: ")
	     ;; (print (first *guesses*))

	     (setf new-population (generation-loop *previous-population*))

	     (setf *previous-population* new-population)
	     (setf new-population (remove-if #'guessed-alreadyp (remove-duplicate-candidates new-population)))
	     ;; (print "Guesses: ")
	     ;; (print *guesses*)
	     ;; (print "New:")


	     ;; Extra info: New population
	     ;; (print "")
	     (format t "~%New population:")
	     (loop for i in new-population
		do (print i))
	     
	     ;; Remove fitness value, turning candidate into guess
	     ;; Pre-pop: (fitness (A B C D))
	     ;; post-pop: ((A B C D))
	     
	     
	     (setf best-guess (choose-best-guess new-population))
	     
	     (push best-guess *guesses*)
	     (format t "~%BEST GUESS CHOSEN: ~a~%" best-guess)

	     ;; debug
	     ;; (print *guesses*)
	     ;(print "Sending next guess")
	    ; (print "")
	     ;; Play guess at top of pile (the most elite)
	     (first (first *guesses*)))))))
