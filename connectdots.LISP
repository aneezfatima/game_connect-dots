(defun connectdots()
                (print '(Please enter the length of the array))
                (setf row_l (read))
                (print '(Please enter the width of the array))
                (setf col_l (read))
                (cond
                 ((null (and (integerp row_l) (integerp col_l))) (print '(Invalid size)))
                ((null (and (> row_l 0) (> col_l 0))) (print '(invalid size)))
                (t
                 (format t "~%The game starts now.~%Please enter the input in the form (alignment row_no. col_no.)~%Alignment should be given as h for horizontal and v for vertical")
                (make-board row_l col_l)
                (run)
                (result))))

(defun make-board(n1 n2) (setf rows (+ 1 (* 2 n1))) (setf cols (+ 1 (* 2 n2)))
                (setf board (make-array (list rows cols) :initial-element " "))
                (putdots board)
		(makenumlist row_l col_l)
                (show board)
                (setf moves (+ (* row_l (+ col_l 1)) (* (+ 1 row_l) col_l))))

(defun putdots(ar)
               (dotimes (row_c (first (array-dimensions ar)) ())
                 (when (= (rem row_c 2) 0)
                   (dotimes (col_c (first (last (array-dimensions ar))) ())
                     (when (= (rem col_c 2) 0)
                       (setf (aref ar row_c col_c) "."))))))


(defun makenumlist(r c)
                    (let ((result1 '()) (result2 '()) (result3 '()) (result4 '()))
                      (setf h-rowlist (dotimes (count (+ r 1) result1)
                                      (setf result1 (cons (+ 1 count) result1))))
                    (setf h-collist (dotimes (count c result2)
                                      (setf result2 (cons (+ 1 count) result2))))
                    (setf v-rowlist (dotimes (count r result3)
                                      (setf result3 (cons (+ 1 count) result3))))
                    (setf v-collist (dotimes (count (+ c 1) result4)
                                      (setf result4 (cons (+ 1 count) result4))))))

(defun show(board)
                (format t (ar2st board)))


(defun ar2st(ar) (let ((result1 ()))
                                 (dotimes (row_c (first (array-dimensions ar)) result1)
                                   (setf result1 (concatenate 'string result1 (concatenate 'string "~%" (let ((result2 ())) (dotimes (col_c (first (last (array-dimensions ar))) result2)
                                                                             (setf result2 (concatenate 'string result2 (aref ar row_c col_c)))))))))))
(defun run()
                     (dotimes (count (- (round (/ moves 2)) (rem moves 2)))
                       (userturn)
                       (computerturn))
                     (when (oddp moves) (userturn)))
(defun userturn()
               (setf turn "A")
               (print '(Your turn. Please enter input))
               (take-input)
               (conip2board input)
               (put-line board_r board_c)
               (process board_r board_c turn)
               (show board))

(defun take-input()
                   (setf input (read))
                   (cond
                    ((null (listp input)) (print '(Invalid form. Please enter again)) (take-input))
                     ((null (and (first input) (second input) (third input))) (format t "NO NIL VALUES ACCEPTED. ENTER AGAIN:") (take-input ))
			((null (member (first input) '(h v))) (print '(invalid alignment. Enter again)) (take-input))
                    ((equal (first input) 'h) 
                     (when (null (member (second input) h-rowlist)) (print '(Invalid row no. Enter again)) (take-input ))
                   (when (null (member (third input) h-collist)) (print '(Invalid column no. Enter again)) (take-input )))
                    ((equal (first input) 'v)
                     (when (null (member (second input) v-rowlist)) (print '(Invalid row no. Enter again)) (take-input ))
                   (when (null (member (third input) v-collist)) (print '(Invalid column no. Enter again)) (take-input ))))
                   (conip2board input)
                   (when (null (equal (aref board board_r board_c) " ")) (print '(These dots are already connected. Enter different input)) (take-input)))


(defun conip2board(ls)
                    (cond
                     ((equal (first input) 'h) (setf board_r (* 2 (- (second input) 1))) (setf board_c (- (* 2 (third input)) 1)))
                     (t (setf board_r (- (* 2 (second input)) 1)) (setf board_c (* 2 (- (third input) 1))))))


(defun put-line(x y)
                    (if (evenp x) (setf (aref board x y) "-")
                      (setf (aref board x y) "|")))


(defun process(x y turn)
               (cond
                ((evenp x) 
                 (when (> (- x 1) 0) (put-letter (- x 1) y turn))
                 (when (< (+ x 1) rows) (put-letter (+ x 1) y turn)))
                ((oddp x)
                 (when (> (- y 1) 0) (put-letter x (- y 1) turn))
                 (when (< (+ y 1) cols) (put-letter x (+ y 1) turn)))))


(defun put-letter(x y turn)
               (when (check x y) (setf (aref board x y) turn)))


(defun check (x y)
              (and (equal (aref board (- x 1) y) "-")
                   (equal (aref board (+ x 1) y) "-")
                   (equal (aref board x (- y 1)) "|")
                   (equal (aref board x (+ y 1)) "|")
		(equal (aref board x y) " ")))

(defun computerturn() (print '(COMPUTER TURN)) (setf turn "C")
                (cond
                 ((grid-with-n-boundaries 3) (put-line (first (first empty)) (second (first empty))) (process (first (first empty)) (second (first empty)) turn))
                 ((grid-with-n-boundaries 1) (put-line (first (first empty)) (second (first empty))))
                 ((grid-with-n-boundaries 0) (put-line (first (first empty)) (second (first empty))))
                 ((grid-with-n-boundaries 2) (put-line (first (first empty)) (second (first empty)))))
                (show board))

(defun grid-with-n-boundaries (n &optional (count1 0))
                (cond
                 ((= count1 row_l) nil)
                 (t (if (grid-with-n-boundaries-aux n count1) t (grid-with-n-boundaries n (+ count1 1))))))

(defun grid-with-n-boundaries-aux (n count1 &optional (count2 0))
                (cond
                 ((= count2 col_l) nil)
                 (t (setf gx (+ (* 2 count1) 1)) (setf gy (+ (* 2 count2) 1))(know-boundaries gx gy)
                    (if (= (length filled) n) t (grid-with-n-boundaries-aux n count1 (+ 1 count2))))))

(defun know-boundaries(x y)
                   (setf empty ())
                   (setf filled ())
                   (if (equal (aref board (- x 1) y) " ") (setf empty (cons (list (- x 1) y) empty)) (setf filled (cons (list (- x 1) y) filled)))
                   (if (equal (aref board (+ x 1) y) " ") (setf empty (cons (list (+ x 1) y) empty)) (setf filled (cons (list (+ x 1) y) filled)))
                   (if (equal (aref board x (- y 1)) " ") (setf empty (cons (list x (- y 1)) empty)) (setf filled (cons (list x (- y 1)) filled)))
                   (if (equal (aref board x (+ y 1)) " ") (setf empty (cons (list x (+ y 1)) empty)) (setf filled (cons (list x (+ y 1)) filled))))


(defun result()
                   (setf c-count 0) (setf a-count 0)
                     (dotimes (count1 row_l c-count)
                       (dotimes (count2 col_l c-count)
                         (if (equal (aref board (+ (* 2 count1) 1) (+ (* 2 count2) 1)) "A") (setf a-count (+ 1 a-count)) (setf c-count (+ 1 c-count)))))
                     (format t "~%SCORE: You -> ~a Computer -> ~a" a-count c-count)
                     (cond ((> a-count c-count) (format t "~%CONGRATULATIONS! YOU WON!!"))
                           ((< a-count c-count) (format t "~%SORRY! YOU LOST!"))
                           (t (format t "~%IT IS A TIE"))))