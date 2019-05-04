;; Team name :   Moonlight Pink Flamingoes
;; Name of file: benchmark.lisp
;; Date created: April 15, 2019
;; Description:  This file provides a benchmarking function that allows for easy retrieval of
;;               timings while playing Mastermind tournaments. This function creates a file
;;               and saves the timings gathered.

;; How to use:
;;   Load the appropriate files
;;     Ex: (load "mastermind.lisp")
;;         (load "baseline-3.lisp")
;;
;;   Define a mastermind game
;;     Ex: (Mastermind 4 6 'ab-color)
;;
;;   Ensure that you save previous benchmark data by changing its name, IT WILL GET OVERWRITTEN
;;   you run this program (Change "benchmark-statistics.txt" to anything else).
;;
;;   Run this benchmark using Mastermind parameters
;;     Ex: (benchmark 'MoonlightPinkFlamingoes 'ab-color 10)
;;
;;   Check directory for benchmark data file.

(defun benchmark (agent-name SCSA number-of-rounds)
  (let ((rounds-won 0)
	(total-score 0)
	(total-real-time 0)
	(total-run-time 0)
	(total-number-of-guesses 0)
	real1 real2 run1 run2 result)
    (with-open-file (stream "benchmark-statistics.txt"
			    :direction :output     ;; write to file
			    :if-exists :supersede  ;; overwrite file if file associated with filename exits
			    :if-does-not-exist :create)
      (format t "~%Benchmark ran with agent [~a] with SCSA [~a] for [~a] rounds:~%~%" agent-name SCSA number-of-rounds)
      (format stream "~%Benchmark ran with agent [~a] with SCSA [~a] for [~a] rounds:~%" agent-name SCSA number-of-rounds)
      (loop for i from 1 to number-of-rounds
	 ;; Get timings
	 do (setf real1 (get-internal-real-time))
	 do (setf run1 (get-internal-real-time))

	 ;; Run tournament round
	 do (format stream "~%Round ~a: ~a~%" i (setf result (play-tournament *Mastermind* agent-name SCSA 1)))

	 ;; Add to average
	 do (setf total-score (+ total-score (first result)))
	   
	 ;; Get second timings
	 do (setf run2 (get-internal-real-time))
	 do (setf real2 (get-internal-real-time))

	 ;; Output
	 do (format stream "Timing:~%")
	 do (format stream "  Round took:~%")
	 do (format stream "      ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))
	 do (format stream "      ~f seconds of run time~%~%" (/ (- run2 run1) internal-time-units-per-second))

	 ;; Collect totals
	 do (setf total-real-time (+ total-real-time
				     (/ (- real2 real1) internal-time-units-per-second)))
	 do (setf total-run-time (+ total-run-time
				    (/ (- run2 run1) internal-time-units-per-second)))
	   
	 do (setf total-number-of-guesses (+ total-number-of-guesses (length *guesses*)))
	 ;;;; Analysis
	 ;; Number of guesses
	 do (format stream "Number of guesses made: ~a~%" (length *guesses*))

	 ;; If win, post winning sequence
	if (< 0 (first result))
	 do (progn
	      (format stream "~%Winning sequence:~%")
	      (loop for i in *guesses*
		 do (format stream "~a~%" i))
	      (setf rounds-won (+ rounds-won 1))))

      ;; Print post tournament metrics
      (format stream "~%Post-tournament metrics:~%  Average score: ~a ~%" (float (/ total-score number-of-rounds)))
      (format stream "  Average real time: ~a~%" (float (/ total-real-time number-of-rounds)))
      (format stream "  Average run time: ~a~%" (float (/ total-run-time number-of-rounds)))
      (format stream "  Average number of guesses: ~a~%" (float (/ total-number-of-guesses number-of-rounds)))
      (format stream "  Rounds won: ~a~%" rounds-won)
      (format stream "  Rounds lost: ~a~%" (- number-of-rounds rounds-won))
      (format stream "  Win rate: ~a~%" (float (/ rounds-won number-of-rounds))))))
	       
  

