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
;;     Ex: (benchmark 'baseline-3-MoonlightPinkFlamingoes 'ab-color 10)
;;
;;   Check directory for benchmark data file.

(defun benchmark (agent-name SCSA number-of-rounds)
  (let (real1 real2 run1 run2)
    (with-open-file (stream "benchmark-statistics.txt"
			    :direction :output     ;; write to file
			    :if-exists :supersede  ;; overwrite file if file associated with filename exits
			    :if-does-not-exist :create)
      (format t "~%Benchmark ran with agent [~a] with SCSA [~a] for [~a] rounds:~%~%" agent-name SCSA number-of-rounds)
      (format stream "Benchmark ran with agent [~a] with SCSA [~a] for [~a] rounds:~%" agent-name SCSA number-of-rounds)
      (loop for i from 1 to number-of-rounds
	 ;; Get timings
	 do (setf real1 (get-internal-real-time))
	 do (setf run1 (get-internal-real-time))

	 ;; Run tournament round
	 do (format stream "Round ~a: ~a~%" i (play-tournament *Mastermind* agent-name SCSA number-of-rounds))

	 ;; Get second timings
	 do (setf run2 (get-internal-real-time))
	 do (setf real2 (get-internal-real-time))

	 ;; Output
	 do (format stream "Timing:~%")
	 do (format stream "  Round took:~%")
	 do (format stream "      ~f seconds of real time~%" (/ (- real2 real1) internal-time-units-per-second))
	 do (format stream "      ~f seconds of run time~%~%" (/ (- run2 run1) internal-time-units-per-second))))))
  

