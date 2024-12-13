(defvar *BUTTON-A-COST* 3)
(defvar *BUTTON-B-COST* 1)
;; Weird condition in the second part of the test require adding 10000000000000 to both axis of the prize.
(defvar *PRIZE-X-OFFSET* 10000000000000)
(defvar *PRIZE-Y-OFFSET* 10000000000000)

(defstruct vec
  x y)

(defstruct solution
  a b)

;; After some pen-and-paper work, I came up with the following formulas:
;; b = (y3 - (y1)a)/y2
;; a = (y2x3 - x2y3)/(y2x1 - x2y1)
;; If either y2 or (y2x1 - x2y1) equals 0, then there is no answer.
;; Additionally, if the result of either of these is not an integer, then the machine is not winnable.
;; In all other cases we simply return 3a + b, but in the case of no answer we return a (solution 0 0).
(defun solve-machine (button-a button-b prize)
  (let ((a-denominator
          (- (* (vec-y button-b) (vec-x button-a))
             (* (vec-x button-b) (vec-y button-a)))))
    (if (= 0 a-denominator (vec-y button-b)) (make-solution :a 0 :b 0)
        (let* ((a
                 (/ (- (* (vec-y button-b) (vec-x prize)) (* (vec-x button-b) (vec-y prize)))
                    a-denominator))
               (b (/ (- (vec-y prize) (* (vec-y button-a) a))
                     (vec-y button-b))))
          (print a)
          (print b)
          (if (and (integerp a) (integerp b)) (make-solution :a a :b b)
              (make-solution :a 0 :b 0))))))

;; HERE is the format of our input data:
;;
;; Button A: X+55, Y+84
;; Button B: X+64, Y+29
;; Prize: X=6049, Y=5045
;;
;; What a horrible format. Who ever thought this was a good idea?  I hope they choke on worms.
;; Anyway, let's think up a good way to parse it.
(defun parse-machine (in)
  (let ((button-a (make-vec))
        (button-b (make-vec))
        (prize (make-vec)))
    (macrolet ((setf-to-read-after-char (c to &rest ctos)
                 `(progn (loop for d = (read-char in nil nil)
                               until (or (null d) (eq ,c d)))
                         (setf ,to (read in nil nil))
                         ,(if (null ctos) nil
                              `(setf-to-read-after-char ,@ctos))))
               (vec-initp (v)
                 `(and (vec-x ,v) (vec-y ,v))))
      (setf-to-read-after-char #\+ (vec-x button-a)
                               #\+ (vec-y button-a)
                               #\+ (vec-x button-b)
                               #\+ (vec-y button-b)
                               #\= (vec-x prize)
                               #\= (vec-y prize))
      (if (and (vec-initp button-a) (vec-initp button-b) (vec-initp prize))
          `((a . ,button-a)
            (b . ,button-b)
            (prize . ,(make-vec :x (+ (vec-x prize) *PRIZE-X-OFFSET*) :y (+ (vec-y prize) *PRIZE-Y-OFFSET*))))
          nil))
    ))

(defun solve (filename)
  (with-open-file (in filename
                      :external-format :utf-8
                      :direction :input)
    (loop for machine = (parse-machine in)
          until (null machine)
          for solution = (solve-machine (cdr (assoc 'a machine))
                                    (cdr (assoc 'b machine))
                                    (cdr (assoc 'prize machine)))
          sum (+ (* 3 (solution-a solution)) (solution-b solution)) into cost
          finally (return cost)
          )))
