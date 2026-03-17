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
             (assert-equal 11 (match '(1 2 3 4) '(6 2 1 5)))
             (assert-equal 40 (match '(1 2 3 4) '(1 2 3 4)))
             (assert-equal 04 (match '(4 3 2 1) '(1 2 3 4))))

(define-test key
            (assert-equal 4621 (key '(4 6 2 1)))
            (assert-equal 1634 (key '(1 6 3 4))))

(define-test set
             (assert-equal () (make-set))
             (assert-equal '(1111 3242 5123 6666)
                           (keys (insert-key 1111
                                       (insert-key 6666
                                                   (insert-key 5123
                                                               (insert-key 3242
                                                                           (make-set))))))))

(define-test number-to-key
             (assert-equal 1111 (number-to-key 0))
             (assert-equal 1112 (number-to-key 1))
             (assert-equal 1121 (number-to-key 6))
             (assert-equal 6666 (number-to-key 1295)))

(define-test codeword
             (assert-equal '(1 3 6 2) (codeword 1362))
             (assert-equal '(6 6 2 4) (codeword 6624)))

(define-test all-keys
             (assert-equal 1296 (length (all-keys))))

(define-test all-possible-results
             (assert-equal '(03 02 01 00 12 11 10 21 20 30) (possible-results 3))
             (assert-equal '(04 03 02 01 00 13 12 11 10 22 21 20 31 30 40) (possible-results 4)))

(define-test match-result-stats
             (let ((stats (match-result-stats 1122 (all-keys))))
               (assert-equal 256 (gethash 0 stats))
               (assert-equal 1 (gethash 40 stats))
               (assert-equal 0 (gethash 31 stats))
               ))

(define-test max-match-result-stats
             (assert-equal 256 (max-result-stats (match-result-stats 1122 (all-keys)))))

(define-test filter-result
             (let ((selection (filter-result 10 1122 (all-keys))))
               (assert-equal 256 (length selection))))
             

(define-test minmax-match-result-stats
             (assert-equal '(1122 2560) (minmax-match-result-stats (all-keys))))

