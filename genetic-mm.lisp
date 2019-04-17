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


(defvar *guesses* nil)
(defvar *previous-population* nil)

;; Generate a population of specified size at random
(defun initialize-population (population-size colors board)
  (loop for i from 1 to population-size
     collect (create-gene-sequence colors board)))

;; Returns fitness value, does not calculate, this is used in sort operations
(defun fitness (candidate)
  (first candidate))

;; Choose random gene
(defun mutation (colors)
    (nth (random (length colors)) colors))
       
;; Mate two guesses and produce an offspring using crossover
(defun mate (parent1 parent2 colors)
  (let (prob)
    (loop for parent1-gene in (second parent1)
       for parent2-gene in (second parent2)
       do (setf prob (random 99))
       if (< prob 45)
       collect parent1-gene
       if (and (>= prob 45) (< prob 90))
       collect parent2-gene
       if (>= prob 90)
       collect (mutation colors))))
  

;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence (colors board)
  (loop for i from 1 to board
     collect (nth (random (length colors)) colors) into generated-candidate
     finally (return (list 0 generated-candidate))))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-top-fifty-candidate (population-size population)
  (nth (random (floor (/ population-size 2))) population))


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
(defun count-color (colors list)
  (loop with tally = (make-array (length colors) :initial-element 0)
     for peg in list
     for index = (spot-color peg)
     do (incf (aref tally index))
     finally (return tally)))


;; Assess candidate score playing against guess
;; Returns candidate score in this format: (blackpegs whitepegs)
;; Modfied function based on "process-guess" method by Professor Susan Epstein
(defun process-candidate-with-guess (candidate guess colors)
  (loop with answer = guess
     with guess-color-count = (count-color colors candidate)
     with true-color-count = (count-color colors answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot-color entry)))
     and do (decf (aref true-color-count (spot-color entry)))
     finally (return (list exact-counter (loop for i from 0 to (1- (length colors))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed)))))

;; Calculate difference of black pegs of candidate c with previous guesses
(defun summate-black-peg-difference (candidate colors)
  (loop for guess in *guesses*
     sum (abs (- (first (process-candidate-with-guess candidate (third guess) colors))
		 (first guess)))))

;; Calculate difference of white pegs of candidate c with previous guesses
(defun summate-white-peg-difference (candidate colors)
  (loop for guess in *guesses*
     sum (abs (- (second (process-candidate-with-guess candidate (third guess) colors))
		 (second guess)))))

;; Calculate fitness by heuristic, described in comment header
(defun calculate-fitness (candidate colors weight-a weight-b board turns-played)
  (+ (* weight-a (summate-black-peg-difference candidate colors))
     (summate-white-peg-difference candidate colors)
     (* weight-b board (1- turns-played))))

;; Return list with elite 10% of population
(defun get-elite-10-percent (population 10-percent)
  (loop for i from 1 to (float 10-percent)
     collect (nth i population)))

;; Return list with mated top 50% to form remaining 90% of population
(defun get-mated-90-percent (population 90-percent colors)
  (let (offspring
	)
    (loop for i from 1 to (float 90-percent)
       do (setf offspring (mate (random-top-fifty-candidate (length population) population)
				(random-top-fifty-candidate (length population) population) colors))
       append (list (list 0 offspring)))))
  
;; Assign fitness values to population
(defun return-population-with-fitness (population colors weight-a weight-b board last-response)
  (loop for candidate in population
     collect (list (calculate-fitness (second candidate)
				      colors
				      weight-a
				      weight-b
				      board
				      (third last-response))
		   (second candidate))))

;; Generate new populations using elitism and mating until reaching max-generations
;; Comb each generation for duplicates and present in previous generation
(defun generation-loop (max-gen population colors weight-a weight-b board last-response)
  (let ((generation population)
	old-generation)
    (loop for i from 1 to max-gen
       do (setf old-generation generation)
       do (setf generation (get-elite-10-percent generation (* 10 (/ (length generation) 100))))
       do (setf generation (append generation
				   (get-mated-90-percent old-generation
							 (* 90 (/ (length old-generation) 100)) colors)))
       do (setf generation (return-population-with-fitness generation
							   colors
							   weight-a
							   weight-b
							   board
							   last-response))
       do (setf generation (sort generation #'< :key #'fitness))
       finally (return generation))))

;; Choose best guess from 

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
	     (setf *previous-population* nil)
	     (setf *guesses* nil)
	     
	     ;; Get the fitness from last-response, place it at (FITNESS (guess))
	     (if (and (= board 4) (= (length colors) 6))
		 (setf guess '(A A B C))
		 (setf guess (second (create-gene-sequence colors board))))
	      ;; For board = 4, color = 6
	     (push (list guess) *guesses*)
	     ;; (print *guesses*)

	     ;; Since first turn, prepare variable for next routine
	     (setf *previous-population* (initialize-population 150 colors board))
	     
	     ;; Play guess, only element in list at this point: ((guess))
	     (first (first *guesses*)))))
	(T
	 (progn
	   (let ((max-generation 100)
		 (weight-a 1)
		 (weight-b 1)
		 population
		 new-population
		 best-guess)
	     ;; Give last guess its result)
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))

	     ;; Extra info: Previous guess
	     ;; (print "")
	     ;; (print "Last guess: ")
	     ;; (print (first *guesses*))

	     (setf population *previous-population*)
	     (setf new-population (generation-loop max-generation
						   population
						   colors
						   weight-a
						   weight-b
						   board
						   last-response))

	     (setf *previous-population* new-population)
	     (setf new-population (remove-if #'guessed-alreadyp (remove-duplicate-candidates new-population)))
	     ;; (print "Guesses: ")
	     ;; (print *guesses*)
	     ;; (print "New:")


	     ;; Extra info: New population
	     ;; (print "")
	      (print "New population:")
	      (loop for i in new-population
   	      do (print i))
	     
	     ;; Remove fitness value, turning candidate into guess
	     ;; Pre-pop: (fitness (A B C D))
	     ;; post-pop: ((A B C D))

	      
	      
	     (pop (first new-population))
	     (push (first new-population) *guesses*)


	     ;; debug
	     ;; (print *guesses*)
	     (print "Sending next guess")
	     (print "")
	     ;; Play guess at top of pile (the most elite)
	     (print (first (first *guesses*)))
	     (print last-response)
	     (first (first *guesses*)))))))
