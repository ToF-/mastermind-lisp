(defparameter *max-pegs* 4)
(defparameter *max-colors* 6)

(defun possible-results (nb-pegs)
  (apply #'append
         (loop for b from 0 to nb-pegs
               collect (loop for w from 0 to (- nb-pegs b)
                             collect (+ (* 10 b) (- (- nb-pegs b) w))))))

(defparameter *results* (possible-results *max-pegs*))

; the number of matching pegs (same color and position) between secret and guess codewords
(defun matches (secret guess)
  (cond
    ((null secret) 0)
    ((= (car secret) (car guess)) (1+ (matches (cdr secret) (cdr guess))))
    (t (matches (cdr secret) (cdr guess)))))

; the number of matching pegs (same color, position indifferent) between secret and guess codewords
(defun hits (secret guess)
  (labels
    ((color-count (color codeword)
                  (cond
                    ((null codeword) 0)
                    ((= color (car codeword)) (1+ (color-count color (cdr codeword))))
                    (t (color-count color (cdr codeword)))))
     (all-color-counts (codeword)
                       (loop for color from 1 to *max-colors*
                             collect (color-count color codeword)))
     (hit-count (secret-counts guess-counts)
                (cond
                  ((null secret-counts) 0)
                  (t (+ 
                       (min (car secret-counts) (car guess-counts))
                       (hit-count (cdr secret-counts) (cdr guess-counts)))))))

    (hit-count (all-color-counts secret) (all-color-counts guess))))

; the number of misplaced pegs (same color, wrong position) between secret and guess codewords
(defun misplaced (secret guess)
    (- (hits secret guess) (matches secret guess)))

; the result eg 01,…,21,…,40 of matching secret and guess codewords
(defun match (secret guess)
  (+ (* 10 (matches secret guess)) (misplaced secret guess)))


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

(defun show-symbols (n s)
  (apply #'concatenate 'string (loop for x from 1 to n collect s)))

(defun show-result (result)
  (cond ((eq 0 result) "__")
        (t (multiple-value-bind (nb-matches nb-misses) (floor result 10)
             (concatenate 'string
                          (show-symbols nb-matches "●")
                          (show-symbols nb-misses "○"))))))

(defun number-to-key (n)
  (defun number-to-key-i (i n)
    (cond ((= 0 i) 0)
          (t (multiple-value-bind (q r) (floor n *max-colors*)
               (+ (* 10 (number-to-key-i (- i 1) q)) (1+ r))))))
  (number-to-key-i *max-pegs* n))

(defun codeword (key)
  (defun codeword-i (i k cw)
    (cond ((= 0 i) cw)
          (t (multiple-value-bind (q r) (floor k 10)
               (codeword-i (- i 1) q (cons r cw))))))
  (codeword-i *max-pegs* key ()))

(defun all-keys ()
  (loop for i
        from 0 to (- (expt *max-colors* *max-pegs*) 1)
        collect (number-to-key i)))

(defun result-to-key (result)
  (+ (* 10 (car result)) (cadr result)))

; make a hash table where each result is a key to a score
(defun make-result-table ()
  (let ((table (make-hash-table)))
      (mapcar #'(lambda (r) (setf (gethash r table) 0)) *results*)
      table))

(defun increment-result (result results)
  (let ((n (gethash result results)))
    (progn
      (setf (gethash result results) (1+ n))
      results)))

(defun match-result-stats (codeword codewords)
  (labels 
    ((increment-match-result-stats (codeword codewords results)
                                   (cond ((null codewords) results)
                                         (t (let ((result (match (codeword codeword)
                                                                   (codeword (car codewords)))))
                                                (increment-match-result-stats codeword (cdr codewords) (increment-result result results)))))))
     (let ((table (make-result-table)))
       (increment-match-result-stats codeword codewords table))))

(defun max-result-stats (stats)
  (apply #'max (mapcar #'(lambda (x) (gethash x stats)) *results*)))

(defun minmax-match-result-stats (codewords)
  (defun minmax-match-result-stats-acc (minimum candidates codewords)
    (cond ((null candidates) minimum)
          (t
            (let ((score (+ (* 10 (max-result-stats (match-result-stats (car candidates) codewords)))
                            (if (null (member (car candidates) codewords)) 1 0)))

                  (current-score (cadr minimum)))
              (if (< score current-score)
                  (minmax-match-result-stats-acc
                    (list (car candidates) score) (cdr candidates) codewords)
                (minmax-match-result-stats-acc
                  minimum (cdr candidates) codewords))))))
    (minmax-match-result-stats-acc (list 0 10000) (all-keys) codewords))

(defun match-codewords (secret guess)
  (match (codeword secret) (codeword guess)))

(defun filter-result (result codeword codewords)
  (remove-if #'(lambda (x) (not (eq (match-codewords codeword x) result))) codewords))

(defun guess-secret (secret)
  (labels
    ((guess-secret-acc (counter candidates)
                       (let* ((minimum (minmax-match-result-stats candidates))
                              (guess (car minimum))
                              (result (match-codewords guess secret)))
                         (progn
                           (format t "~A) ~A : ~A~%" counter guess (show-result result))
                           (cond 
                             ((> counter 5) (format t "I can't in 5 guesses~%"))
                             ((eq 40 result) (format t "found in ~A guesses~%" counter))
                             (t (guess-secret-acc (1+ counter) (filter-result result guess candidates))))))))
    (let* ((guess 1122)
           (result (match-codewords guess secret)))
      (progn
        (format t "~A) ~A : ~A~%" 1 1122 (show-result result))
        (guess-secret-acc 2 (filter-result result 1122 (all-keys)))))))

