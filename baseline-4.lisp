;; Team:         Moonlight Pink Flamingoes
;; Name of File: baseline-4.lisp
;; Description:  Baseline-4 agent implements Rao's algorithm to the Mastermind game.
;;               The comment explanations of functions "Update" and "GetNext", as well as their helper functions,
;;               are from "AN ALGORITHM TO PLAY THE GAME OF MASTERMIND" by T. Magadeva Rao.

(defvar *inferences*)
(defvar *trial*)
(defvar *being-considered*)
(defvar *being-fixed*)
(defvar *positions*)
(defvar *color*)
(defvar *gain*)


;;;;
;;;; GETNEXT Algorithm constructs the next trial arrangement
;;;;
;;;  Psuedo code
;; Begin
;;  For i := 1 to N do ;; N is board
;;  Case
;;   Tied(i)
;;    : gi := itscolor (i)
;;   (i = nextpos(beingfixed))  ;; position i in guess is the next position being fixed
;;    : gi := beingfixed
;;   (Length(inferences) = N)
;;    : gi = secondunfixed(inferences);
;;   else
;;    : gi = beingconsidered
;;  end
;; end 

(defun getnext (board)
  (setf *trial* nil)
  (loop for i from 1 to board
     do (cond
	   ((tied i) (cons (itscolor i) *trial*))
	   ((= i (nextpos *being-fixed*)) (cons *being-fixed* *trial*))
	   ((= (length *inferences*) (length *trial*)) (cons (second-unfixed) *trial*))
  	   (T (cons *being-considered* *trial*))))
  (setf *trial* *trial*))
;;;;
;;;; GETNEXT helpers
;;;;
;;   Tied(pos): boolean; Returns true if the position pos has a color tied to it
;;   Example:
;;         (A (1)) = true, (A (1 2)) = false
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
  (cond
    ((< 1 (length *inferences*))
     (loop for sublist in *inferences*
	when (equal (first sublist) color)
	return (first (second sublist))))
    (T 0)))
       
;; Secondunfixed : colors, Returns the second color which is not yet fixed
(defun second-unfixed ()
  (loop for sublist in *inferences*
     when (> (length (second sublist)) 1)
     collect (first sublist) into unfixed
     finally (return (second unfixed))))


(defun update (board colors last-response)
  (break)
  (if (numberp *being-fixed*)
      (addlists (- (+ (first last-response) (second last-response)) (numfix) 1))
      (addlists (- (+ (first last-response) (second last-response)) (numfix))))
  (case (second last-response)
    (0 (progn
	 (fix)
	 (bump)))
    (1 (progn
	 (if (not (equal nil *being-fixed*))
	     (del *being-fixed* *being-considered*))
	 (del *being-fixed* *being-fixed*)))
    (2 (fix1 *being-considered* *being-fixed*))
    (otherwise (report-error-update)))
  (cleanup)
  (nextcolor colors))

;;;;
;;;; UPDATE HELPERS 
;;;;

;; Simple report error function
(defun report-error-update ()
  "ERROR IN UPDATE ROUTINE")

;;   Addlists(gain, beingconsidered): Adds sublists (equal in number to gain) to the list inferences,
;;                                                each sublist with header 'beingconsidered'
(defun addlists (gain)
  (loop for counter from 1 to gain
     do (setf *inferences* (push (list *being-considered* *positions*) *inferences*)))
  (setf *inferences*  *inferences*))

;;   Fix (beingfixed): 'Fix'es the beingfixed in its next possible position and deletes appropriate position from
;;                      other lists. For example, if beingfixed = 3, and inferences is:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;                      Fixing of 3 would result in
;;                                              ((3 (2))
;;                                               (4 (3 4 5))
;;                                               (7 (1 3 4 5))
(defun fix ()
  (let ((position-to-remove (nextpos *being-fixed*)))
  (loop for sublist in *inferences*
     when (equal (first sublist) *being-fixed*)
     do (setf (second sublist) (list (first (second sublist)))))
  (loop for sublist in *inferences*
     when (not (equal (first sublist) *being-fixed*))
     do (setf (second sublist) (remove position-to-remove (second sublist))))))

;;   Bump(beingfixed): BeingFixed will get an updated value. If beingfixed = 3 and our inferences look like:
;;                                              ((3 (2 4 5))
;;                                               (4 (2 3 4 5))
;;                                               (7 (1 2 3 4 5))
;;   then 3 the nextpos of 3 will become 4.
(defun bump ()
  (loop for sublist in *inferences*
     when (equal (first sublist) *being-fixed*)
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
  (setf *being-considered* (nth *color* colors))
  (setf *color* (+ *color* 1)))

;;   Numfix returns the number of positions tied to a color
(defun numfix ()
  (loop for sublist in *inferences*
     when (= (length (second sublist)) 1)
     collect sublist into fixed
     finally (return (length fixed))))

(defun initialize (board colors)
  (setf *being-considered* (first colors))
  (setf *color* 1)
  (setf *inferences*  '())
  (setf *being-fixed* 0)
  (setf *being-considered* (first colors))
  (setf *positions* '())
  (setf *trial* '())
  
  (loop for i from 1 to board
     do (push i *positions*)
     do (push *being-considered* *trial*))
  (setf *positions* (reverse *positions*))
  (setf *trial* *trial*))

;; Main routine function
(defun baseline-4-MoonlightPinkFlamingoes (board colors SCSA last-response)
  (declare (ignore SCSA))
  (cond
    ((null last-response) (initialize board colors))
    (T
     (update board colors last-response)
     (getnext board)
     *trial*)))

