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
(defun getnext ()
  (loop for i in *trial*
       (cond ((tied i) (setf i (itscolor i)))
 	     ((= i (nextpos beingfixed)) (setf i 'beingfixed))
 	     ((= (length *inferences*) (length trial)) (setf i (secondunfixed *inferences*)))
 	     (t (setf i 'beingconsidered)))))

;;;;
;;;; GETNEXT helpers
;;;;
;;   Tied(pos): boolean; Returns true if the position pos has a color tied to it
;;   Example:
;;         ((A (1)))
(defun tied (pos)
  (loop for sublist in *inferences*
       ;; Loop through inferences until found, otherwise nill
       thereis (and (member pos (second sublist)) (= (length (second sublist)) 1))))

;;   Itscolor(pos) : colors; When pos is a tied position returns the color to which i is tied
(defun itscolor (pos)
  (loop for sublist in *inferences*
     ;; Loop through inferences until found, otherwise nill
     when (and (member pos (second sublist)) (= (length (second sublist)) 1))
     return (first sublist)))

;;   Nextpos(i): positions; Returns the next possible position for color i. That is if i = 3
;;                          and the corresponding sublist is (3 (2 4 5)) then it returns 2.
(defun nextpos (color)
  (loop for sublist in *inferences*
     when (equal (first sublist) color)
     return (first (second sublist))))
       
;; Secondunfixed : colors, Returns the second color which is not yet fixed
(defun secondunfixed ()
  (loop for sublist in *inferences*
     when (> (length (second sublist)) 1)
     collect (first sublist) into unfixed
     finally (return (second unfixed))))

;;;;
;;;; UPDATE algorithm updates knowledge base
;;;;
(defun update (last-response gain colors)
  (if (= beingfixed 0)
      (setf gain (- (+ (first last-response) (second last-response)) (numfix) 1))
      (setf gain (- (+ (first last-response) (second last-response)) (numfix))))
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
  (nextcolor beingconsidered colors))

;;;;
;;;; UPDATE HELPERS 
;;;;

;; Simple report error function
(defun report-error-update ()
  "ERROR IN UPDATE ROUTINE")

;;   Addlists(gain, beingconsidered): Adds sublists (equal in number to gain) to the list inferences,
;;                                                each sublist with header 'beingconsidered'
(defun addlists (gain beingconsidered)
  (loop for counter from 1 to gain
     do (setf *inferences* (append *inferences* (list (list beingconsidered (list nil)))))))

;;   Fix (beingfixed): 'Fix'es the beingfixed in its next possible position and deletes appropriate position from
;;                      other lists. For example, if beingfixed = 3, and inferences is:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;                      Fixing of 3 would result in
;;                                              ((3 (2))
;;                                               (4 (3 4 5))
;;                                               (7 (1 3 4 5))
(defun fix (beingfixed)
  (let ((position-to-remove (nextpos beingfixed)))
  (loop for sublist in *inferences*
     when (equal (first sublist) beingfixed)
     do (setf (second sublist) (list (first (second sublist)))))
  (loop for sublist in *inferences*
     when (not (equal (first sublist) beingfixed))
     do (setf (second sublist) (remove position-to-remove (second sublist))))))

;;   Bump(beingfixed): BeingFixed will get an updated value. If beingfixed = 3 and our inferences look like:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;   then 3 the nextpos of 3 will become 4.
(defun bump (beingfixed)
  (loop for sublist in *inferences*
     when (equal (first sublist) beingfixed)
     do (setf (second sublist) (rest (second sublist)))))

;;   Del(i j): Deletes the current position of the color i from the sublist for color j. For example, if i=2, j=3
;;             and the sublists are
;;             (2 (3 4 5))
;;             (3 (2 3 4 5))
(defun del (i j)
  (loop for sublist in *inferences*
     when (equal (first sublist) j)
     do (setf (second sublist) (remove (nextpos i) (second sublist)))))

;;   Fix1(i j); fixes the color i in the current position of the color j. In the above example, if i=3 and j=2,
;;              then the color 3 gets tied to Position 3, its sublist becomes (3 (3)), and the positon 3 gets
;;              deleted from the other sublists
(defun fix1 (i j)
  (let ((position-to-remove (nextpos j)))
    (loop for sublist in *inferences*
       when (equal (first sublist) i)
       do (setf (second sublist) (list position-to-remove)))
    (loop for sublist in *inferences*
       when (not (equal (first sublist) i))
       do (setf (second sublist) (remove position-to-remove (second sublist))))))

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
(defun cleanup ()
  (loop for sublist in *inferences*
     do (loop for sublist2 in (remove sublist *inferences*)
	   do (setf (second sublist2) (remove (first (second sublist)) (second sublist2))))))

;;   Nextcolor(): Gets a new color for beingconsidered. If all the five colors have already
;;                been detected then beingconsidered is simply set to zero
(defun nextcolor (colors)
  (loop for sublist in *inferences*
     do (setf colors (remove (first sublist) colors))
     finally (return (first colors))))

;;   Numfix returns the number of positions tied to a color
(defun numfix ()
  (loop for sublist in *inferences*
     when (= (length (second sublist)) 1)
     collect fixed
       finally (return (length fixed))))

;; Main routine function
(defun baseline-4-MoonlightPinkFlamingoes (board colors SCSA last-response)
  ;; Rao's algorithm main routine:
  ;; Update ()
  ;; Getnext (trial)
  
  ;; Update the inferences
  (update last-response 1 colors)
  
  ;; Get next guess
  (getnext)
  
  ;; return new guess
  *trial*)

