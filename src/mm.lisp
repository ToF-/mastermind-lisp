; mm.lisp

(load "src/mastermind")

(let ((rs (make-random-state t)))
  (guess-secret (number-to-key (random (expt *max-colors* *max-pegs*) rs))))
(quit)
