;; Team:         Moonlight Pink Flamingoes
;; Name of File: baseline-4.lisp
;; Description:  Baseline-4 agent implements Rao's algorithm to the Mastermind game.
;;               The comment explanations of functions "Update" and "GetNext", as well as their helper functions,
;;               are from "AN ALGORITHM TO PLAY THE GAME OF MASTERMIND" by T. Magadeva Rao.

;; Keeps track of inferences
(defvar *inferences*)

;; Previous guess
(defvar *trial*)

;; Getnext Algorithm constructs the next trial arrangement
(defun getnext (trial)
  (loop for i in trial
       (cond ((tied i) (setf i (itscolor i)))
 	     ((= i (nextpos beingfixed)) (setf i 'beingfixed))
 	     ((= (length *inferences*) (length trial)) (setf i (secondunfixed *inferences*)))
 	     (t (setf i 'beingconsidered)))))

;;;;
;;;; GETNEXT helpers
;;;;
;;   Tied(pos): boolean; Returns true if the position pos has a color tied to it
;;   Example:
;;     Inference list = ((C1 (P1 P2)) (C2 (P2 P3))
;;     (tied 4) => false, not color is tied to position 4
(defun tied (pos)
  (loop for sublist in *inferences*
       ;; Loop through inferences until found, otherwise nill
       thereis (not (null (member pos (second sublist))))))

;;   Itscolor(pos) : colors; When pos is a tied position returns the color to which i is tied
(defun itscolor (pos)
  (loop for sublist in *inferences*
     ;; Loop through inferences until found, otherwise nill
     when (not (null (member pos (second sublist))))
     ;; Return color
     return (first sublist)))

;;   Nextpos(i): positions; Returns the next possible position for color i. That is if i = 3
;;                          and the corresponding sublist is (3 (2 4 5)) then it returns 2.
(defun nextpos (color)
  (loop for sublist in *inferences*
     when (equal (first sublist) color)
     return (first (second sublist))))
       
;; Secondunfixed : colors, Returns the second color which is not yet fixed
(defun secondunfixed ()
  ;; Uses global *inferences* as argument
  )

;;;;
;;;; UPDATE algorithm updates knowledge base
;;;;
(defun update (inferences bulls cows gain)
  (if (= beingfixed 0)
      (setf gain (- (+ bulls cows) (numfix *inferences*) 1))
      (setf gain (- (+ bulls cows) (numfix *inferences*))))
  (case cows
    (0 (progn
	 (fix beingfixed)
	 (bump beingfixed)))
    (1 (progn
	 (if () ; beingfixed <> 0
	     (del beingfixed beingconsidered))
	 (del beingfixed beingconsidered)))
    (2 (fix1 beingconsidered beingfixed))
    (otherwise (report-error-update)))
  (cleanup *inferences*)
  (nextcolor beingconsidered))

;;;;
;;;; UPDATE HELPERS 
;;;;

;; Simple report error function
(defun report-error-update ()
  "ERROR IN UPDATE ROUTINE")

;;   Addlists(gain, beingconsidered, inferences): Adds sublists (equal in number to gain) to the list inferences,
;;                                                each sublist with header 'being considered'
(defun addlists ())

;;   Fix (beingfixed): 'Fix'es the beingfixed in its next possibleposition and deletes appropriate position from
;;                      other lists. For example, if beingfixed = 3, and inferences is:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;                      Fixing of 3 would result in
;;                                              ((3 (2))
;;                                               (4 (3 4 5))
;;                                               (7 (1 3 4 5))
(defun fix ())

;;   Bump(beingfixed): BeingFixed will get an updated value. In the above example, beingfixed will become 4.
(defun bump ())

;;   Del(i j): Deletes the current position of the color i from the sublist for color j. For example, if i=2, j=3
;;             and the sublists are
;;             (2 (3 4 5))
;;             (3 (2 3 4 5))
(defun del ())

;;   Fix1(i j); fixes the color i in the current position of the color j. In the above example, if i=3 and j=2,
;;              then the color 3 gets tied to Position 3, its sublist becomes (3 (3)), and the positon 3 gets
;;              deleted from the other sublists
(defun fix1 ())

;;   Cleanup(inferences); cleans up the inferences list. For example, if inferences was:
;;                        ((2 (3))
;;                         (4 (3 4 5))
;;                         (5 (3 5))
;;                        cleanup will transform this to
;;                        ((2 (3))
;;                         (4 (4 5))
;;                         (5 (5))
;;                        and then finally to
;;                        ((2 (3))
;;                         (4 (4))
;;                         (5 (5))
;(defun cleanup (inferences))

;;   Nextcolor(beingconsidered): Gets a new color for being considered. if all the five colors have already
;;                               been detected then being considered is simple set to zero
(defun nextcolor ())

;; Main routine function
(defun baseline-4-MoonlightPinkFlamingoes (board color SCSA last-response)
  ;; Update (inferences)
  ;; Getnext (trial)
  ;; Numfix (inferences): Numfix simply returns the number, in this case the nubmer of inferences
  
  ;; Update the inferences
  (update *inferences*)
  
  ;; Get next guess
  (getnext trial)
  
  ;; return new guess
  trial)

