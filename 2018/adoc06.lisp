;;;; https://adventofcode.com/2018/day/6
(load "common.lisp")

(defvar *example-input*)
(setq *example-input*
      "1, 1
      1, 6
      8, 3
      3, 4
      5, 5
      8, 9")

;; 1, 1  A  inf
;; 1, 6  B  inf
;; 8, 3  C  inf
;; 3, 4  D
;; 5, 5  E
;; 8, 9  F  inf

(defvar *input*)
(setf *input*
      "177, 51
      350, 132
      276, 139
      249, 189
      225, 137
      337, 354
      270, 147
      182, 329
      118, 254
      174, 280
      42, 349
      96, 341
      236, 46
      84, 253
      292, 143
      253, 92
      224, 137
      209, 325
      243, 195
      208, 337
      197, 42
      208, 87
      45, 96
      64, 295
      266, 248
      248, 298
      194, 261
      157, 74
      52, 248
      243, 201
      242, 178
      140, 319
      69, 270
      314, 302
      209, 212
      237, 217
      86, 294
      295, 144
      248, 206
      157, 118
      155, 146
      331, 40
      247, 302
      250, 95
      193, 214
      345, 89
      183, 206
      121, 169
      79, 230
      88, 155")

(defun parse-input-to-points-list (input)
  (setf input (split-string-by input #\Newline :trim-whitespace t))
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

(defstruct boundary
  x-max
  x-min
  y-max
  y-min)

(defun determine-boundary (points)
  (let* ((first-point-x (caar points))
         (first-point-y (cadar points))
         (x-max first-point-x)
         (x-min first-point-x)
         (y-max first-point-y)
         (y-min first-point-y))

    (dolist (point points)
      (destructuring-bind (x y) point
        (setf x-max (max x-max x))
        (setf x-min (min x-min x))
        (setf y-max (max y-max y))
        (setf y-min (min y-min y))))

    (make-boundary
      :x-max (1+ x-max)
      :x-min (1- x-min)
      :y-max (1+ y-max)
      :y-min (1- y-min))))

(defun is-on-boundary (point boundary)
  (destructuring-bind (x y) point
    (or
      (= x (boundary-x-min boundary))
      (= x (boundary-x-max boundary))
      (= y (boundary-y-min boundary))
      (= y (boundary-y-max boundary)))))

(defun print-areas (boundary areas)
  (let ((point-to-char (make-hash-table :test 'equal)))

    (let ((current-char #\a))
      (loop for point being the hash-key of areas
            using (hash-value area-points) do

            (format t "~% ~a -> ~a" point current-char)

            (dolist (area-point area-points)
              (setf (gethash area-point point-to-char) current-char))

            (if (char-equal #\z current-char)
              (setf current-char #\a)
              (setf current-char (code-char (1+ (char-int current-char)))))))

    (format t "~%")

    (loop for y
          from (boundary-y-min boundary)
          to   (boundary-y-max boundary) do

          (loop for x
                from (boundary-x-min boundary)
                to   (boundary-x-max boundary) do

                (let* ((point (list x y))
                       (point-char (gethash point point-to-char)))
                  (if point-char
                    (if (gethash point areas)
                      (format t "~a" (char-upcase point-char))
                      (format t "~a" point-char))
                    (format t "."))))

          (format t "~%"))))


(defun solve1 (boundary points)
  (let ((infinite-areas (make-hash-table :test 'equal))
        (areas (make-hash-table :test 'equal)))

    (loop for x
          from (boundary-x-min boundary)
          to   (boundary-x-max boundary) do
          (loop for y
                from (boundary-y-min boundary)
                to   (boundary-y-max boundary) do

                (let ((current-point (list x y))
                      (distance)
                      (min-distance)
                      (distances (make-hash-table :test 'equal)))

                  (dolist (destination-point points)
                    (setf distance (calculate-manhattan-distance
                                     current-point
                                     destination-point))
                    (if min-distance
                      (setf min-distance (min min-distance distance))
                      (setf min-distance distance))
                    (setf (gethash destination-point distances) distance))

                  (let* ((closest-points
                           (loop for point being the hash-key of distances
                                 using (hash-value distance-to-point)
                                 when (= distance-to-point min-distance)
                                 collect point))
                         (closest-point (car closest-points)))
                    (if (= 1 (length closest-points))
                      (progn
                        (if (gethash closest-point areas)
                          (push current-point (gethash closest-point areas))
                          (setf (gethash closest-point areas) (list current-point)))
                        (if (is-on-boundary current-point boundary)
                          (setf (gethash closest-point infinite-areas) t))))))))


    (let ((area-size)
          (max-area-size 0)
          (max-area-point))

      (loop for point being the hash-key of areas
            using (hash-value area-points) do
            (if (not (gethash point infinite-areas))
              (progn
                (setf area-size (length area-points))
                (if (> area-size max-area-size)
                  (progn
                    (setf max-area-size area-size)
                    (setf max-area-point point))))))

      max-area-size)))

(defun distance-between-points (start end)
  (destructuring-bind (sx sy) start
    (destructuring-bind (ex ey) end
      (+ (abs (- sx ex))
         (abs (- sy ey))))))

(defun solve2 (boundary points &key (threshold 10000))
  (let ((area-size 0))

    (loop for x
          from (boundary-x-min boundary)
          to   (boundary-x-max boundary) do
          (loop for y
                from (boundary-y-min boundary)
                to   (boundary-y-max boundary) do

                (let ((point (list x y))
                      (distances-sum 0))
                  (dolist (destination-point points)
                    (setf distances-sum
                          (+ distances-sum (distance-between-points
                                             point
                                             destination-point))))
                  (if (< distances-sum threshold)
                    (incf area-size)))))

    area-size))

(let* ((points (parse-input-to-points-list *input*))
       (boundary (determine-boundary points)))
  (print (solve1 boundary points))
  (print (solve2 boundary points)))


