(require racket/base) ;;This allows the type system to work.
(require (file "cs230-graphics.scm")) ;;Pull in the definitions for the drawing window and stuff. Assumes the file is in the same directory. 
;; Here are the procedures you will modify in the problem set

(define side ;;generator
  (lambda ((length <real>) (heading <real>) (level <integer>))
    (if (zero? level)
        (drawto heading length)
        (let ((len/3 (/ length 3))
              (lvl-1 (- level 1)))
          (side len/3 heading lvl-1)
          (side len/3 (- heading PI/3) lvl-1)
          (side len/3 (+ heading PI/3) lvl-1)
          (side len/3 heading lvl-1)))))

(define snowflake:0 ;;initiator
  (lambda ((length <real>) (level <integer>))
    (side length 0.0 level)
    (side length (* 2 PI/3) level)
    (side length (- (* 2 PI/3)) level)))

;;Problem 1
(define flip-side
  (lambda ((length <real>) (heading <real>) (level <integer>))
    (if (zero? level) 
        (drawto heading length)
        (let ((len/2 (/ length (sqrt 2)))
              (lvl-1 (- level 1)))
          (flip-side len/2 (- heading PI/4) lvl-1)
          (flip-side length (+ heading PI/4) lvl-1)
          (flip-side len/2 (- heading PI/4) lvl-1)))))

(define square-snowflake:1
  (lambda ((length <real>) (level <integer>))
    (flip-side length 0.0 level)
    (flip-side length PI/2 level)
    (flip-side length (* 2 PI/2) level)
    (flip-side length (- PI/2) level)))

;;Problem 2

(define square-snowflake:2
  (lambda ((length <real>) (level <integer>) (generator <function>))
    (generator length 0.0 level)
    (generator length PI/2 level)
    (generator length (* 2 PI/2) level)
    (generator length (- PI/2) level)))

(define snowflake:2 ;;initiator
  (lambda ((length <real>) (level <integer>) (generator <function>))
    (generator length 0.0 level)
    (generator length (* 2 PI/3) level)
    (generator length (- (* 2 PI/3)) level)))



;;Problem 3
(define snowflake-inv 
  (lambda ((length <real>) (level <integer>) (generator <function>) (inverter <function>))
    (generator length 0.0 level inverter)
    (generator length (* 2 PI/3) level inverter)
    (generator length (- (* 2 PI/3)) level inverter)))


(define side-inv 
  (lambda ((length <real>) (heading <real>) (level <integer>) (inverter <function>))
    (if (zero? level)
        (drawto heading length)
        (if (= (inverter level) -1)
            (let ((len/3 (/ length 3))
                  (lvl-1 (- level 1)))
              (side-inv len/3 heading lvl-1 inverter)
              (side-inv len/3 (+ heading PI/3) lvl-1 inverter)
              (side-inv len/3 (- heading PI/3) lvl-1 inverter)
              (side-inv len/3 heading lvl-1 inverter))
            (let ((len/3 (/ length 3))
                  (lvl-1 (- level 1)))
              (side-inv len/3 heading lvl-1 inverter)
              (side-inv len/3 (- heading PI/3) lvl-1 inverter)
              (side-inv len/3 (+ heading PI/3) lvl-1 inverter)
              (side-inv len/3 heading lvl-1 inverter))))))

;;Problem 4: Calculating the Length of the Snowflake
(define snowflake-length
  (lambda ((length <real>) (level <integer>) (generator <function>) (inverter <function>))
    (+ (generator length 0.0 level inverter)
       (generator length (* 2 PI/3) level inverter)
       (generator length (- (* 2 PI/3)) level inverter))))


(define side-length 
  (lambda ((length <real>) (heading <real>) (level <integer>) (inverter <function>))
    (if (zero? level)
        length ;;I think this is wrong? 
        (if (= (inverter level) -1)
            (let ((len/3 (/ length 3))
                  (lvl-1 (- level 1)))
              (+ (side-length len/3 heading lvl-1 inverter)
                 (side-length len/3 (+ heading PI/3) lvl-1 inverter)
                 (side-length len/3 (- heading PI/3) lvl-1 inverter)
                 (side-length len/3 heading lvl-1 inverter)))
            (let ((len/3 (/ length 3))
                  (lvl-1 (- level 1)))
              (+ (side-length len/3 heading lvl-1 inverter)
                 (side-length len/3 (- heading PI/3) lvl-1 inverter)
                 (side-length len/3 (+ heading PI/3) lvl-1 inverter)
                 (side-length len/3 heading lvl-1 inverter)))))))

(init-graphics 640 480)
(clear)
(moveto 100 100)

(snowflake-inv 150 3 side-inv
               (lambda ((level <integer>)) 
	         (if (odd? level) 1 -1)))
