; mm.lisp

(load "src/mastermind")

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    (progn 
      (format t "~A~%" args)
      (if (eq 2 (length args))
        (guess-secret (parse-integer (cadr args)))
        (let ((rs (make-random-state t)))
          (guess-secret (number-to-key (random (expt *max-colors* *max-pegs*) rs))))))))

(main)
(quit)
