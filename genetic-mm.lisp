;; Team name:    Moonlight Pink Flamingoes
;; Date created: April 8th, 2019
;; Description: Genetic algorithm implementation for Mastermind based on the paper "Efficient solutions for Mastermind using genetic algorithms" by Berghman, Goossens, and Leus.


;; Symbol list
;; P -> pegs
;; Xi -> bulls / black pegs
;; Yi -> cows / white pegs
;; Ei -> population at cycle i
;; (E hat i )- > subset of population E at cycle i
;; c -> candidate
;; h -> generation counter
;; maxgen -> max loops/generation cycles
;; maxsize -> max size of population
;;;;;;;;;;;; Psuedo code
;; 1. Set i = 1
;; 2. Play fixed initial guess g1:
;; 3. Get response Xi and Yi;
;; 4. while Xi =/= P do
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN AGENT ROUTINE
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN AGENY ROUTINE
;;   14. Get response Xi (bulls) Yi (cows)
;; end while

;; population format (fitness (guess))

;; 2
(defvar *guesses*)

(defvar *previous-generation*)

;; 7
(defun initialize-population (population-size colors board)
  (loop for i from population-size
     append (create-gene-sequence colors board)))

;; Returns fitness value, does not calculate, this is used to sort
(defun fitness (populant)
  (first populant))

;; 9
;; Choose random gene
(defun mutation (colors)
    (nth (random (length colors) colors)))
       
;; Can be expanded to more than just one operator
(defun mate (parent1 parent2 colors)
  (loop for parent1-gene in (second parent1)
     for parent2-gene in (second parent2)
     if (< (random 99) 45)
     append parent1-gene
     if (and (>= (random 99) 45) (< (random 99) 90))
     append parent2
     if (> (random 99) 90)
     append (mutation colors))))


;; Create a string of genes (colors) at random
(defun create-gene-sequence (colors board)
  (loop for i from 1 to board
       append (nth (random (length colors)))))

(defun genetic-agent (board colors SCSA last-response)
  ;; Make five random initial guesses
  (if (< (third last-response) 4)
      (progn
	(let (guess)
	  ;; Get the fitness from last-response, place it at (FITNESS (guess))
	  (setf (second (first *guesses*)) (third last-response))
	  (setf guess (create-gene-sequence))
	  (push guess *guesses*)
	  (push guess *previous-population*)
	  ;; Play guess
	  (second (first *guesses*))))
      ;; Main genetic algorithm loop after first five random guesses
      (progn
	(let ((population-size 10)
	      (generation 1)
	      population
	      new-population
	      10-percent-of-size
	      90-percent-of-size)

	  ;; Give last guess its fitness
	  (setf (second (first *guesses*)) (third last-response))
	  
	  ;; Generate initial population
	  (setf population (initialize-population population-size colors board))

	  ;; Calculate value, TBD, for now rely on game to give fitness 
	  
	  ;; Sort by assigned fitness value
	  (sort #'fitness population)
	  
	  ;; Pick top 10% of previous (sorted) population and place in new population
	  (setf 10-percent-of-size (* 10 (/ population-size 100)))
	  (loop for i from 1 to 10-percent-of-size
	     do (setf new-population (append new-population (nth i population))))

	  ;; Pick the top 50% and mate them, take the resulting offspring to the next generation
	  (setf 90-percent-of-size (* 90 (/ population-size 100)))
	  (loop for i from 1 to 90-percent-of-size
	     append (mate (nth (random (/ population 2)) population)
			  (nth (random (/ population 2)) population) colors))

	  ;; Recompute fitness and sort again
	  (sort #'fitness new-population)
	  (setf *previous-population* new-population)

	  ;; Save next guess
	  (push (second (first new-population)) *guesses*)
	  
	  ;; Play guess at top of pile (the most elite)
	  (second (first new-population))))))
