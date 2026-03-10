(require :asdf)
(require :lisp-unit)
(in-package :lisp-unit)
(setq *print-failures* t)

(load "test/mastermind-test")

(run-tests :all)
(sb-ext:quit)
