(require :asdf)
(require :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)
(load "src/mastermind")

(define-test matches
             (assert-equal 4 (matches '(1 1 1 1) '(1 1 1 1)))
             (assert-equal 3 (matches '(1 1 2 1) '(1 1 1 1)))
             (assert-equal 0 (matches '(4 2 1 3) '(6 6 6 6)))
             )

(define-test misplaced
            (assert-equal 2 (misplaced '(1 2 3 4) '(4 3 6 6)))
            (assert-equal 4 (misplaced '(1 1 2 2) '(2 2 1 1))))

(define-test match
             (assert-equal '(1 1) (match '(1 2 3 4) '(6 2 1 5)))
             (assert-equal '(4 0) (match '(1 2 3 4) '(1 2 3 4)))
             (assert-equal '(0 4) (match '(4 3 2 1) '(1 2 3 4))))
