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

;; 2
(defvar *guesses* nil)

(defvar *previous-population* nil)

;; Generate a population of specified size at random
(defun initialize-population (population-size colors board)
  (loop for i from 1 to population-size
     collect (create-gene-sequence colors board)))

;; Returns fitness value, does not calculate, this is used in sort operations
(defun fitness (candidate)
  (first candidate))

;; 9
;; Choose random gene
(defun mutation (colors)
    (nth (random (length colors)) colors))
       
;; Mate two guesses and produce an offspring
;; Can be expanded to more than just one operator
;; Currently uses mutation and crossover based on probability
(defun mate (parent1 parent2 colors)
  (loop for parent1-gene in (second parent1)
     for parent2-gene in (second parent2)
     if (< (random 99) 45)
     collect parent1-gene
     if (and (>= (random 99) 45) (< (random 99) 90))
     collect parent2-gene
     if (> (random 99) 90)
     collect (mutation colors)))


;; Create a candidate/candidate using genes (colors) at random
(defun create-gene-sequence (colors board)
  (loop for i from 1 to board
     collect (nth (random (length colors)) colors) into generated-candidate
       finally (return (list 0 generated-candidate))))

;; For use in list sorted by fitness, give random candidate position in top 50% of population
(defun random-top-fifty-candidate (population-size population)
  (nth (random (/ population-size 2)) population))



(defun genetic-agent (board colors SCSA last-response)
  (declare (ignore SCSA))
  ;; Make initial guess
  (cond ((null last-response)
	 (progn
	   (let (guess)
	     ;; Get the fitness from last-response, place it at (FITNESS (guess))
	     (setf guess (list '(A A B C)))
	     (push guess *guesses*)
	     (push guess *previous-population*)

	     ;; debug
	     ;; (print *guesses*)
     	     (print (first (first *guesses*)))
	     
	     ;; Play guess, only element in list at this point: ((guess))
	     (first (first *guesses*)))))
	(T
	 (progn
	   (let ((population-size 10)
		 ;; Reccommended by paper to limit generations to 100
		 ;; (generation 100)
		 ;; Recommended by paper to limit population to 60
		 ;; (population 60)
		 population
		 new-population
		 10-percent-of-size
		 90-percent-of-size)
	     ;; Give last guess its result
	     ;; ... Push white pegs
	     (push (second last-response) (first *guesses*))
	     ;; ... Push black pegs
	     (push (first last-response) (first *guesses*))

	     
	     ;; Generate initial population
	     ;;(print "Generating random population...")
	     (setf population (initialize-population population-size colors board))

	     ;; Calculate value, TBD, for now rely on game to give fitness 
	     
	     ;; Sort by assigned fitness value
	     (setf population (sort population #'> :key #'fitness))
	     ;; Pick top 10% of previous (sorted) population and place in new population
	     ;;(print "Performing elitsm...")
	     (setf 10-percent-of-size (* 10 (/ population-size 100)))
	     (loop for i from 1 to 10-percent-of-size
		do (setf new-population (append new-population (nth i population))))
	     ;; (print "Mating...")
	     ;; Pick the top 50% and mate them, take the resulting offspring to the next generation
	     (setf 90-percent-of-size (* 90 (/ population-size 100)))
	     (loop for i from 1 to 90-percent-of-size
		append (mate (random-top-fifty-candidate population-size population)
			     (random-top-fifty-candidate population-size population) colors) into offspring
			     finally (setf new-population (append new-population offspring)))
	     
	     ;; Recompute fitness and sort again
	     (setf new-population (sort population #'> :key #'fitness))
	     (setf *previous-population* population)

	     
	     ;; Choose most elite guess
	     ;; Remove fitness value, turning candidate into guess

	     ;; Pre-pop: (fitness (A B C D))
	     ;; post-pop: ((A B C D))
	     (pop (first new-population))
	     (push (first new-population) *guesses*)
	     
	     ;; debug
	     ;; (print *guesses*)
	     ;;(print "Sending next guess")
	     ;(print *guesses*)
	     ;; Play guess at top of pile (the most elite)
	     (first (first *guesses*)))))))
