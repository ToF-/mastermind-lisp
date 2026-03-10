(defparameter *max-pegs* 4)
(defparameter *max-colors* 6)

(defun matches (secret guess)
  (cond
    ((null secret) 0)
    ((= (car secret) (car guess)) (1+ (matches (cdr secret) (cdr guess))))
    (t (matches (cdr secret) (cdr guess)))))

(defun counter (n codeword)
  (cond
    ((null codeword) 0)
    ((= n (car codeword)) (1+ (counter n (cdr codeword))))
    (t (counter n (cdr codeword)))))

(defun all-counter (codeword)
  (loop for i from 1 to 10 collect (counter i codeword)))

(defun hit-counter (secret-counter guess-counter)
  (cond
    ((null secret-counter) 0)
    (t (+ (min (car secret-counter) (car guess-counter))
          (hit-counter (cdr secret-counter) (cdr guess-counter))))))

(defun hits (secret guess)
  (hit-counter (all-counter secret) (all-counter guess)))

(defun misplaced (secret guess)
    (- (hits secret guess) (matches secret guess)))

(defun match (secret guess)
  (list (matches secret guess) (misplaced secret guess)))

(defun key (codeword)
  (defun key-acc (n codeword)
    (cond
      ((null codeword) n)
      (t (key-acc (+ (* 10 n) (car codeword)) (cdr codeword)))))
  (key-acc 0 codeword))

(defun make-set ()
  ())

(defun insert-key (key set)
  (setf set (adjoin key set)))

(defun keys (set)
        (sort set #'<))


(defun number-to-key (n)
  (defun number-to-key-i (i n)
    (cond ((= 0 i) 0)
          (t (multiple-value-bind (q r) (floor n *max-colors*)
               (+ (* 10 (number-to-key-i (- i 1) q)) (1+ r))))))
  (number-to-key-i *max-pegs* n))

(defun key-to-codeword (key)
  (defun key-to-codeword-i (i k cw)
    (cond ((= 0 i) cw)
          (t (multiple-value-bind (q r) (floor k 10)
               (key-to-codeword-i (- i 1) q (cons r cw))))))
  (key-to-codeword-i *max-pegs* key ()))

(defun all-keys ()
  (loop for i
        from 0 to (- (expt *max-colors* *max-pegs*) 1)
        collect (number-to-key i)))

(defun result-to-key (result)
  (+ (* 10 (car result)) (cadr result)))

(defun make-result-table ()
  (let ((results (make-hash-table)))
    (progn
      (setf (gethash 04 results) 0)
      (setf (gethash 03 results) 0)
      (setf (gethash 02 results) 0)
      (setf (gethash 01 results) 0)
      (setf (gethash 13 results) 0)
      (setf (gethash 12 results) 0)
      (setf (gethash 11 results) 0)
      (setf (gethash 10 results) 0)
      (setf (gethash 22 results) 0)
      (setf (gethash 21 results) 0)
      (setf (gethash 20 results) 0)
      (setf (gethash 31 results) 0)
      (setf (gethash 30 results) 0)
      (setf (gethash 40 results) 0)
      results)))

(defun increment-result (result results)
  (let ((n (gethash result results)))
    (setf (gethash result results) (1+ n))))

(defun match-result-stats (codeword codewords)
  (defun increment-match-result-stats (codeword codewords results)
    (cond ((null codewords) results)
          (t (let ((result (result-to-key
                             (match (key-to-codeword codeword)
                                    (key-to-codeword (car codewords))))))
               (increment-result result results)))))
  (increment-match-result-stats codeword codewords (make-result-table)))
