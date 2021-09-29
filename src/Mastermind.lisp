;;;*******************************************************************************************
;;;THIS FILE DEFINES THE GAME ENVIRONMENT AND PROVIDES A SAMPLE PLAYER THAT MAKES RANDOM MOVES
;;;*******************************************************************************************

;;Support functions
;selects a random element from a list
(defun random-chooser (list)
  (nth (random (length list)) list))

;selects n distinct random elements from list)
(defun choose-n-random (n list)
  (if (> n (length list)) 
      (print 'error)
      (loop for i from 1 to n
	 for choices = (copy-list list) then (set-difference choices (list chosen))
	 for chosen = (random-chooser choices)
	 collect chosen)))

;returns the first number items of list
(defun firstn (number list)
  (loop for i from 1 to number
     for item in list
     collect item))

;converts color letters to numbers
;notice that T cannot be a letter here, so TT is used instead
(defun spot (color)
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

;;;*******************************************************************************************
;;Class definition and *Mastermind*
;;;*******************************************************************************************

;board is the number of pegs
;colors is a list of letters (for colors)
;colors is the number of colors in the game
;answer is the secret code
;SCSA is the name of the secret code generator
;guesses is how many guesses so far
;game-cutoff is the number of guesses permitted
;tournament-cutoff is the number of games in a tournament

;Mastermind;note default values for all slots 
(defclass game ()
  ((board :initarg :board :initform 0 :accessor board :documentation "number of pegs")
   (colors :initarg :colors :initform '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z) :accessor colors :documentation "list of possible colors")
   (number-of-colors :initarg :number-of-colors :initform 0 :accessor number-of-colors :documentation "number of colors used")
   (answer :initarg :answer :initform nil :accessor answer :documentation "target the current round is trying to guess")
   (SCSA :initarg :SCSA :initform nil :accessor SCSA :documentation "name of generator for targets")
   (guesses :initarg :guesses :initform 0 :accessor guesses :documentation "history of guesses")
   (game-cutoff :initarg :game-cutoff :initform 100 :accessor game-cutoff :documentation "time after which a round is terminated")))

(defvar *Mastermind* (make-instance 'game))

;Creates a game called *Mastermind* on pegs pegs with colors colors
;secret code is created during tournament
(defun Mastermind (pegs hues SCSA)
  (declare (special *Mastermind*)) 
  (when (and (integerp pegs) (plusp pegs) (integerp hues) (plusp hues) (<= hues 26)) 
    (setf *Mastermind* (make-instance 'game :board pegs :number-of-colors hues :SCSA SCSA
				      :colors (firstn hues '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z))))))

;;;*******************************************************************************************
;;PLAY
`;;;*******************************************************************************************

;returns t if a guess is of the correct length and only contains valid colors, else nil
(defmethod valid-guess ((self game) guess)
  (and (listp guess) 
       (= (length guess) (board self))
       (loop with colors = (colors self) for item in guess
	    always (member item colors :test 'equal))))

;counts the number of each color in a guess into an array and returns the array
(defmethod color-counter ((self game) list)
  (loop with tally = (make-array (number-of-colors self) :initial-element 0)
     for peg in list
     for index = (spot peg)
     do (incf (aref tally index))
     finally (return tally)))

;scores a guess, returning a two-element list (#exact #other) where other means "right color, wrong location"
(defmethod process-guess ((self game) guess)
  (loop with answer = (answer self)
     with guess-color-count = (color-counter self guess)
     with true-color-count = (color-counter self answer)
     with exact-counter = 0
     for entry in guess
     for peg in answer
     for exact = (equal entry peg)
     when exact 
     do (incf exact-counter)
     and do (decf (aref guess-color-count (spot entry)))
     and do (decf (aref true-color-count (spot entry)))
     finally (progn
	      ;(print "answer")
	      ;(print answer)
	      (return (list exact-counter (loop for i from 0 to (1- (number-of-colors self))
					    for guessed = (aref true-color-count i)
					    for true = (aref guess-color-count i)
					    when (<= true guessed)
					    sum true
					    else sum guessed))))))
;if the guess is the answer this returns the list (win #guesses), if the guess is invalid it returns nil
;otherwise it returns the guess followed by the score (#exact #other #guesses-so-far)
(defmethod respond-to-guess ((self game) guess i)
  (let* ((board (board self))
	 (valid (valid-guess self guess))
	 (score (when valid (process-guess self guess)))
	 (win (and score (= (first score) board))))
    (cond ((not valid)  nil)
	  (win (list 'win i))
	  (t (append score (list i))))))

;plays one round of game against team during which team gets up to game-cutoff guesses at the answer
;uncomment the format statements for a play-by-play version
;calls a function team that you will provide that makes a guess
;the name of your function should be the name of your team
;each round is limited to 10,000,000 microseconds (unlimited time for a single guess)
;if you time out it counts as if your last guess scored (0 0)
(defmethod play-round ((self game) team)
  (declare (special *Mastermind*)) 
  (loop with game-cutoff = (game-cutoff self)
     with stop-time = (+ 5000000 (get-internal-run-time))
     with board = (board *Mastermind*)
     with colors = (colors *Mastermind*)
     with SCSA = (SCSA *Mastermind*)
     for i from 1 to game-cutoff
     for guess = (funcall team board colors SCSA nil) then (funcall team board colors SCSA response)
     for response = (respond-to-guess self guess i)
     for win = (equal (first response) 'win)
     for time-is-up = (> (get-internal-run-time) stop-time)
     ;do (print (list (get-internal-run-time) stop-time))
     ;when win
     ;do (format t "~%Win. Round over.")
     ;else when response
     ;do (format t "~%score ~a" response)
     ;else do (format t "~%Invalid entry. Round over.")
     until (or win (null response) (= i game-cutoff) time-is-up)
     finally (return (cond (time-is-up '(0 0)) 
			   ((null response) nil)
			   (t response)))))

;Plays tournament-length rounds against one team
;a win is worth more if it takes fewer guesses (division by square root of number of guesses)
;ATTENTION: there are two ways to use this function.
;METHOD 1: If argument is a fixed list of pre-generated answers: Set the value of a variable to an SCSA-generated list and use that variable
;for example: (setf foo (SCSA-sampler 25 'two-color-alternating 7 5)) and then call (play-tournament *Mastermind* team-name foo 25)
;will rerun your team-name on the same data every time and help you debug
;METHOD 2: If argument is an SCSA: Generate a new list of boards on every run.
;for example, (play-tournament *Mastermind* team-name 'two-color-alternating 25))
;will run your team-name against a different set of freshly-generated codes every time, and help you do performance evaluation
;WARNING: if you use method 1, be sure you generate enough boards for your call to play-tournament (e.g., at least 25 here)
(defmethod play-tournament ((self game) team argument number-of-games)
  (declare (special *Mastermind*)) (print team)
  (loop with wins = 0
     with failures = 0
     with losses = 0
     with codes = (if (listp argument) argument (SCSA-sampler number-of-games argument (board self) (number-of-colors self)))
     for i from 0 to (1- number-of-games)
     for round = (and (setf (answer *Mastermind*) (nth i codes)) (play-round self team)) ;this is where the; code is set
    ;do (print (nth i codes))
    ; when (= (* 10 (floor (/ i 10))) i) do (print i)
    ;do (print (list wins losses failures))
     when (equal (first round) 'win)
     do (incf wins (float (/ 1 (expt (second round) .5)))) 
     else when (null round)
     do (incf failures) 
     else do (incf losses)
     finally (progn
	       (print (answer *Mastermind*))
	       (print (list 'score (scoring-function (list wins losses failures)))))
       (return (list wins losses failures))))

;you get 5 points for a full win and -2 for every round that ended in an invalid guess
(defun scoring-function (list)
  (+ (* 5 (first list)) (* -2 (third list))))
       
;;;*******************************************************************************************
;;Sample SCSAs
;;;*******************************************************************************************

;to see what codes from a specific SCSA look like, use this function
;for example (scsa-sampler 100 'first-and-last 6 8) will give 100 samples for 6 pegs with 8 colors using the SCSA first-and-last
;collects and returns number of secret codes generated by SCSA for pegs pegs and colors colors
(defun SCSA-sampler (number SCSA pegs colors)
  (loop for i from 1 to number
       collect (funcall SCSA pegs (firstn colors '(A B C D E F G H I J K L M N O P Q R S TT U V W X Y Z)))))

;makes a list of length length containing colors selected at random from colors
(defun insert-colors (length colors)
  (loop for i from 1 to length
     collect (random-chooser colors)))

;makes a 2-color list 
(defun two-color (length colors)
  (loop with choices = (choose-n-random 2 colors)
     for i from 1 to length 
     collect (random-chooser choices)))

;makes a list of only As and Bs
(defun ab-color (length colors)
  (declare (ignore colors))
  (loop with choices = '(A B)
     for i from 1 to length
     collect (random-chooser choices)))

;makes a list that alternates 2 colors
(defun two-color-alternating (length colors)
  (loop with choices = (choose-n-random 2 colors) 
     with first-color = (first choices)
     with second-color = (second choices)
     for i from 1 to length 
     when (oddp i) 
     collect first-color
     else collect second-color))  

;makes a list in which a color appears at most once
(defun only-once (length colors)
  (if (< (length colors) length) 
      ;(break)
      (loop for i from 1 to length
	 for color-list = (copy-list colors) then (set-difference color-list (list choice))
	 for choice = (random-chooser color-list)
	 collect choice)))

;makes a list in which the first and last colors are the same
(defun first-and-last (length colors)
  (loop with first = (random-chooser colors)
       for i from 2 to (1- length)
       collect (random-chooser colors) into ans
       finally (return (append (cons first ans) (list first)))))

;makes a list that usually has fewer (2 or 3) colors
(defun usually-fewer (length colors)
  (let* ((probability (random 100))
	 (coin-flip (when (< probability 90) (random-chooser '(2 3))))
	 (choices (cond (coin-flip (if (= 3 coin-flip) (choose-n-random 3 colors) (choose-n-random 2 colors)))
			(t colors))))
    (loop for i from 1 to length
       collect (random-chooser choices))))

;makes a list with preferences for fewer colors
(defun prefer-fewer (length colors)
  (let* ((probability (random 100))
	 (color-count (length colors))
	 (coin-flip (cond ((<= probability 49) 1)
		      ((<= probability 74) 2)
		      ((<= probability 87) (if (>= color-count 3) 3 color-count))
		      ((<= probability 95) (if (>= color-count 4) 4 color-count))
		      ((<= probability 98) (if (>= color-count 5) 5 color-count))
		      (t (if (>= color-count 6) (random-chooser (loop for i from 6 to color-count 
								   collect i)) color-count))))
	 (choices (choose-n-random coin-flip colors))) 
    (loop for i from 1 to length
       collect (random-chooser choices))))

;;;*******************************************************************************************
;;Sample teams
;;;*******************************************************************************************
;this is a really dumb team... it makes random guesses
(defun RandomFolks (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
  (insert-colors board colors))

;this isn't much better.... it guesses all the same color, and chooses that color at random
(defun Boring (board colors SCSA last-response)
  (declare (ignore SCSA last-response))
    (make-list board :initial-element (random-chooser colors)))
