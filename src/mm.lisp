; mm.lisp

(load "src/mastermind")

(format t "~A~%" (matches '(1 2 3 4) '(3 2 1 5)))
(format t "~A~%" (matches '(1 2 3 4) '(1 2 1 5)))
(format t "~A~%" (misplaced '(1 2 3 4) '(4 3 6 6)))
(format t "~A~%" (match '(1 2 3 4) '(4 3 6 6)))
