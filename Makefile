tests: test/mastermind-test.lisp src/mastermind.lisp
	sbcl --load ~/.sbclrc --script test/all-tests.lisp

mastermind: src/mastermind.lisp src/mm.lisp
	sbcl --noinform --load ~/.sbclrc --script src/mm.lisp
