;; Team:         Moonlight Pink Flamingoes
;; Name of File: baseline-4.lisp
;; Description:  Baseline-4 agent implements Rao's algorithm to the Mastermind game.
;;               The comment explanations of functions "Update" and "GetNext", as well as their helper functions,
;;               are from "AN ALGORITHM TO PLAY THE GAME OF MASTERMIND" by T. Magadeva Rao.

;; Keeps track of inferences out of scope
(defvar *inferences*)

;; Getnext Algorithm constructs the next trial arrangement
;; Helpers:
;;   Tied(i): boolean; Returns true if the position i has a color tied to it
;;   Itscolor(i): colors; When i is a tied position returns the color to which i is tied
;;   Length(L): integer; Returns the length, the number of top-level elements in a list L
;;   Nextpos(i): positions; Returns the next possible position for color i. That is if i = 3
;;                          and the corresponding sublist is (3 (2 4 5)) then it returns 2.
;;   Secondunfixed(inferences): colors; Returns the second color which is not yet fixed.
(defun getnext (trial)
  (loop for i in trial
       (cond ((tied i) (setf i (itscolor i)))
	     ((= i (nextpos beingfixed)) (setf i 'beingfixed))
	     ((= (length *inferences*) (length trial)) (setf i (secondunfixed(*inferences*))))
	     (t (setf i 'beingconsidered)))))

;; getnext helpers
(defun tied ())
(defun itscolor ())
(defun nexpos ())

;; Update algorithm updates knowledge base
;;   Addlists(gain, beingconsidered, inferences): Adds sublists (equal in number to gain) to the list inferences,
;;                                                each sublist with header 'being considered'
;;   Fix (beingfixed): 'Fix'es the beingfixed in its next possibleposition and deletes appropriate position from
;;                      other lists. For example, if beingfixed = 3, and inferences is:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;                      Fixing of 3 would result in
;;                                              ((3 (2))
;;                                               (4 (3 4 5))
;;                                               (7 (1 3 4 5))
;;                    
;;   Bump(beingfixed): BeingFixed will get an updated value. In the above example, beingfixed will become 4.
;;
;;   Del(i j): Deletes the current position of the color i from the sublist for color j. For example, if i=2, j=3
;;             and the sublists are
;;             (2 (3 4 5))
;;             (3 (2 3 4 5))
;;   
;;   Fix1(i j); fixes the color i in the current position of the color j. In the above example, if i=3 and j=2,
;;              then the color 3 gets tied to Position 3, its sublist becomes (3 (3)), and the positon 3 gets
;;              deleted from the other sublists
;;
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
;;   Nextcolor(beingconsidered): Gets a new color for being considered. if all the five colors have already
;;                               been detected then being considered is simple set to zero
;; Main routine function
(defun baseline-4-MoonlightPinkFlamingoes (board color SCSA last-response)
  ;; Update (inferences)
  ;; Getnext (trial)
  ;; Numfix (inferences): Numfix simply returns the number, in this case the nubmer of inferences
  
  
  )

