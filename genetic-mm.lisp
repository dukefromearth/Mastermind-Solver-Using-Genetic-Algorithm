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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAIN AGENT ROUTINE
;;   14. Get response Xi (bulls) Yi (cows)
;; end while

;;1
(defvar *i*)

;; 2
(defvar *guess*)

;; 7
(defun initialize-population ())

;; 9
(defun crossover ())

;; 9
(defun mutate ())

;; 9
(defun inversion ())

;; 9
(defun permutation ())

;; 10
(defun calculate-fitness ())

(defun genetic-agent (board colors SCSA last-response)
  (let ((maxgen 100)    ;; arbitrary numbers
	(maxsize 250)   ;; ...
	population
	new-population)   
    
    ;; Initialize iteration counter
    (if (null i)
	(progn
	  (setf i 1) ;; Initialize counter
	  (setf *guess* (A A B C))) ;; Reccommended initial guess
	(progn
	  (setf *i* (+ *i* 1))
	  (setf population (initialize-population))
	  (loop for h from 1 to maxgen) ;; and size of population less than eq to maxsize
	  )

    *guess*))
