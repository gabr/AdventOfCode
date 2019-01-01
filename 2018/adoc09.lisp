;;;; https://adventofcode.com/2018/day/9
(load "common.lisp")

(defvar *test-data*)
(setf *test-data*
      '(
        ("9 players; last marble is worth 25 points" 32)
        ("17 players; last marble is worth 1104 points" 2764)
        ("10 players; last marble is worth 1618 points" 8317)
        ("13 players; last marble is worth 7999 points" 146373)
        ("21 players; last marble is worth 6111 points" 54718)
        ("30 players; last marble is worth 5807 points" 37305)))

(defvar *input*)
(setf *input* "404 players; last marble is worth 71852 points")

(defun parse-input (input)
  (let ((splited (split-string-by input #\Space)))
    (strings-list-to-numbers-list
      (list (nth 0 splited) (nth 6 splited)))))

(defun test (solve-function test-data)
  (dolist (td test-data)
    (destructuring-bind (input test-result) td
      (let ((parsed-input (parse-input input)))
        (destructuring-bind (players-count last-marble-worth) parsed-input
          (let ((solve-result (funcall solve-function players-count last-marble-worth)))
            (if (= solve-result test-result)
              (format t "OK   '~A' expected: ~6A  = got: ~A~%"
                      input test-result solve-result)
              (format t "FAIL '~A' expected: ~6A != got: ~A~%"
                      input test-result solve-result))))))
    ;(return-from test)
    ))

(defun solve1 (players-count last-marble-worth)
  (let ((marbles (list 0))
        (marbles-count 1)
        (players (make-array players-count :initial-element 0))
        (current-player-index -1)
        (current-marble-worth 1)
        (*print-circle* t))
    ; circular marbles list
    (setf (cdr (last marbles)) marbles)
    ;; loop till the last-marble-worth
    (loop
      ;; determine current player index
      (setf current-player-index
            (mod (1+ current-player-index) players-count))
      ;; two types of marble handling
      (if (= (mod current-marble-worth 23) 0)
        ;; special case for marbles with worth multiple by 23
        (progn
          ; increment player score by marble worth
          (incf (aref players current-player-index) current-marble-worth)
          ; move current marble 7 places to the left
          (setf marbles (nthcdr (- marbles-count 7) marbles))
          ; remove this marble and add it to the marbles list
          ;(format t "Before remove: ~A~%" marbles)
          (incf (aref players current-player-index)
                (pop marbles))
          ; remove poped element from tail of circular list
          (setf (cdr (nthcdr (- marbles-count 2) marbles)) nil)
          (decf marbles-count)
          ; circular marbles list
          (setf (cdr (last marbles)) marbles)
          ;(format t "After remove: ~A~%" marbles)
          ;(format t "Players scores updated: ~A~%" players)
          )
        ;; regular case
        (progn
          ; push current marble after next marble and update count
          (push current-marble-worth (cddr marbles))
          (incf marbles-count)
          ; move move marbles reference to the current marble
          (if (= 2 marbles-count)
            (setf marbles (cdr marbles))
            (setf marbles (cddr marbles)))
          ;(format t "Marbles: ~A~%" marbles)
          ))
      ;(format t "[~A]: ~A~%" (1+ current-player-index) marbles)
      ;; end loop condition
      (if (= current-marble-worth last-marble-worth)
        ; end loop if all marbles where calculated
        (return)
        ; otherwise increment marble worth
        (incf current-marble-worth)))

    ;; return the greatest player score
    ;(format t "Players scores: ~A~%" players)
    ;(format t "Current marble worth: ~A~%" current-marble-worth)
    (loop for score across players
          maximize score into max-score
          finally (return max-score))))


;(test #'solve1 *test-data*)

(destructuring-bind (players-count last-marble-worth) (parse-input *input*)
  (format t "~A~%" (solve1 players-count last-marble-worth))
  ; 3653994575
  (format t "~A~%" (solve1 players-count (* 100 last-marble-worth))))

