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
