;; -*- lexical-binding: t; -*-

;; Acronym
(defun acronym (phrase)
  (upcase
   (mapconcat
    (lambda (s) (substring s 0 1))
    (split-string phrase "[ _-]+"))))

(string= (acronym "Thank George It's Friday!") "TGIF")
(string= (acronym "First In, First Out") "FIFO")


;; Armstrong Numbers
(defun armstrong-p (number)
  (let*
      ((number-repr (number-to-string number))
       (digits-count (length number-repr))
       (digits-list (mapcar 'string-to-number
                            (split-string number-repr "" t)))
       (series-args (lambda (i) (expt i digits-count)))
       (series-result (apply '+ (mapcar series-args digits-list))))
    (equal series-result number)))

(equal (armstrong-p 153) t)
(equal (armstrong-p 154) nil)


;; Binary Search
(defun find-binary (array value)
  (let ((low-index 0)
        (high-index (1- (length array)))
        result-index)
    (while (and (<= low-index high-index) (not result-index))
      (let* ((mid-index (/ (+ low-index high-index) 2))
             (mid-value (aref array mid-index)))
        (cond ((< mid-value value) (setq low-index  (1+ mid-index)))
              ((> mid-value value) (setq high-index (1- mid-index)))
              (t (setq result-index mid-index)))))
    result-index))

(equal (find-binary [1 3 4 6 8 9 11] 6) 3)
(equal (find-binary [] 6) nil)


;; Collatz Conjecture
(defun steps (number)
  (let ((result-steps 0))
    (if (> number 0)
        (while (/= number 1)
          (setq number
                (if (= (mod number 2) 0) (/ number 2)
                  (1+ (* 3 number))))
          (setq result-steps (1+ result-steps)))
      (error "value-error: argument must be positive number"))
    result-steps))

(equal (steps 1000000) 152)
(condition-case result-error (steps 0) (error (message "t")))


;; Difference of Squares
(defun sum-of-squares (n)
  (/ (* n (+ n 1) (+ (* 2 n) 1)) 6))

(defun square-of-sum (n)
  (expt (/ (* n (1+ n)) 2) 2))

(defun difference (n)
  (- (square-of-sum n) (sum-of-squares n)))

(equal (difference 10) 2640)
(equal (difference 100) 25164150)
