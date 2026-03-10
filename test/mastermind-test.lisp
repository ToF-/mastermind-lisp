(require :asdf)
(require :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)
(load "src/mastermind")

(define-test dummy
             (assert-equal 4 (+ 2 2)))
