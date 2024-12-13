(defvar *BUTTON-A-COST* 3)
(defvar *BUTTON-B-COST* 1)

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
