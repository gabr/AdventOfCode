;;;; https://adventofcode.com/2018/day/6
(load "common.lisp")

(defvar *example-input*)
(setq *example-input* "1, 1
                       1, 6
                       8, 3
                       3, 4
                       5, 5
                       8, 9")

(defun parse-input-to-coordinates-list (input)
  (setf input (split-string-by *example-input* #\Newline :trim-whitespace t))
  (loop for i in input
        collect (strings-list-to-numbers-list
                  (split-string-by i #\, :trim-whitespace t))))

(defun calculate-manhattan-distance (start end)
  "Calculates Manhattan distance between
  two given points in form of (x y)."
  (let* ((x-delta (abs (- (car start) (car end))))
         (y-delta (abs (- (cadr start) (cadr end))))
         (min-delta (min x-delta y-delta))
         (max-delta (max x-delta y-delta)))
    (+ (* 2 min-delta) (- max-delta min-delta))))

(let ((input (parse-input-to-coordinates-list *example-input*)))
  (dolist (i input)
    (format t "~a -> (0 0) = ~a~%"
            i
            (calculate-manhattan-distance '(0 0) i))))


