;;;; https://adventofcode.com/2018/day/7
(load "common.lisp")

(defvar *example-input*)
(setf *example-input*
      "Step C must be finished before step A can begin.
      Step C must be finished before step F can begin.
      Step A must be finished before step B can begin.
      Step A must be finished before step D can begin.
      Step B must be finished before step E can begin.
      Step D must be finished before step E can begin.
      Step F must be finished before step E can begin.")

(defvar *input*)
(setf *input*
      "Step P must be finished before step F can begin.
      Step F must be finished before step M can begin.
      Step Q must be finished before step S can begin.
      Step K must be finished before step G can begin.
      Step W must be finished before step X can begin.
      Step V must be finished before step I can begin.
      Step S must be finished before step Y can begin.
      Step U must be finished before step D can begin.
      Step J must be finished before step B can begin.
      Step Z must be finished before step C can begin.
      Step Y must be finished before step D can begin.
      Step X must be finished before step A can begin.
      Step E must be finished before step N can begin.
      Step M must be finished before step B can begin.
      Step N must be finished before step I can begin.
      Step I must be finished before step T can begin.
      Step H must be finished before step A can begin.
      Step A must be finished before step B can begin.
      Step O must be finished before step L can begin.
      Step T must be finished before step L can begin.
      Step D must be finished before step R can begin.
      Step G must be finished before step L can begin.
      Step C must be finished before step R can begin.
      Step R must be finished before step L can begin.
      Step L must be finished before step B can begin.
      Step O must be finished before step R can begin.
      Step Q must be finished before step I can begin.
      Step M must be finished before step L can begin.
      Step R must be finished before step B can begin.
      Step J must be finished before step O can begin.
      Step O must be finished before step B can begin.
      Step Y must be finished before step L can begin.
      Step G must be finished before step R can begin.
      Step P must be finished before step Z can begin.
      Step K must be finished before step Y can begin.
      Step X must be finished before step I can begin.
      Step E must be finished before step H can begin.
      Step I must be finished before step H can begin.
      Step P must be finished before step K can begin.
      Step G must be finished before step B can begin.
      Step H must be finished before step L can begin.
      Step X must be finished before step C can begin.
      Step P must be finished before step X can begin.
      Step X must be finished before step M can begin.
      Step Q must be finished before step H can begin.
      Step S must be finished before step Z can begin.
      Step C must be finished before step B can begin.
      Step N must be finished before step A can begin.
      Step M must be finished before step R can begin.
      Step X must be finished before step E can begin.
      Step P must be finished before step L can begin.
      Step H must be finished before step G can begin.
      Step E must be finished before step D can begin.
      Step D must be finished before step L can begin.
      Step W must be finished before step A can begin.
      Step S must be finished before step X can begin.
      Step V must be finished before step O can begin.
      Step H must be finished before step B can begin.
      Step T must be finished before step B can begin.
      Step Y must be finished before step C can begin.
      Step A must be finished before step R can begin.
      Step N must be finished before step L can begin.
      Step V must be finished before step Z can begin.
      Step W must be finished before step V can begin.
      Step S must be finished before step M can begin.
      Step Z must be finished before step A can begin.
      Step W must be finished before step S can begin.
      Step Q must be finished before step R can begin.
      Step N must be finished before step G can begin.
      Step Z must be finished before step L can begin.
      Step K must be finished before step O can begin.
      Step X must be finished before step R can begin.
      Step V must be finished before step H can begin.
      Step P must be finished before step R can begin.
      Step M must be finished before step A can begin.
      Step K must be finished before step L can begin.
      Step P must be finished before step M can begin.
      Step F must be finished before step N can begin.
      Step W must be finished before step H can begin.
      Step K must be finished before step B can begin.
      Step H must be finished before step C can begin.
      Step X must be finished before step H can begin.
      Step V must be finished before step U can begin.
      Step S must be finished before step H can begin.
      Step J must be finished before step X can begin.
      Step S must be finished before step N can begin.
      Step V must be finished before step A can begin.
      Step H must be finished before step O can begin.
      Step Y must be finished before step O can begin.
      Step H must be finished before step R can begin.
      Step X must be finished before step T can begin.
      Step J must be finished before step H can begin.
      Step G must be finished before step C can begin.
      Step E must be finished before step R can begin.
      Step W must be finished before step J can begin.
      Step F must be finished before step E can begin.
      Step P must be finished before step I can begin.
      Step F must be finished before step T can begin.
      Step J must be finished before step L can begin.
      Step U must be finished before step Z can begin.
      Step Q must be finished before step D can begin.")

(defstruct node
  name
  dependencies
  produces
  produced
  production-step
  required-production-steps)

(defun parse-input (input &key (print-debug nil))
  (let ((names (make-hash-table :test 'equal))
        (mapped-pairs (make-hash-table :test 'equal))
        (nodes-by-name (make-hash-table :test 'equal)))

    ;; gather all nodes names and map dependencies
    (loop for line in (split-string-by input #\Newline :trim-whitespace t) do
          (let* ((split (split-string-by line #\Space))
                 (l (char (nth 1 split) 0))
                 (r (char (nth 7 split) 0)))
            (setf (gethash l names) t)
            (setf (gethash r names) t)
            (if (gethash l mapped-pairs)
              (push r (gethash l mapped-pairs))
              (setf (gethash l mapped-pairs) (list r)))))

    ;; create nodes with simple dependencies names lists
    (loop for name being the hash-key of names do
          (setf (gethash name nodes-by-name)
                (make-node
                  :name name
                  :produced nil
                  :dependencies (loop for k being the hash-key of mapped-pairs
                                      using (hash-value v)
                                      when (find name v :test 'equal)
                                      collect k)
                  :produces (gethash name mapped-pairs))))

    (if print-debug
      (loop for node being the hash-value of nodes-by-name do
            (print node)))

    ;; connect nodes by replacind dependencies
    ;; strings list with nodes lists
    (loop for node being the hash-value of nodes-by-name do
          (if (node-dependencies node)
            (setf (node-dependencies node)
                  (loop for dependency-name in (node-dependencies node)
                        collect (gethash dependency-name nodes-by-name))))
          (if (node-produces node)
            (setf (node-produces node)
                  (loop for produces-name in (node-produces node)
                        collect (gethash produces-name nodes-by-name)))))

    ;; return nodes list
    (loop for node being the hash-value of nodes-by-name
          collect node)))

(defun node-can-be-produces (node)
  (if (node-dependencies node)
    (progn
      (dolist (d (node-dependencies node))
        (if (not (node-produced d))
          (return-from node-can-be-produces nil)))
      t))
  t)

(defun get-nodes-which-can-be-produced (nodes)
  (sort
    (loop for n in nodes
          when (and (node-can-be-produces n)
                    (not (node-produced n)))
          collect n)
    #'string<
    :key #'node-name))

(defun solve1 (nodes)
  (let ((node))
    (loop
      (setf node (car (get-nodes-which-can-be-produced nodes)))

      (if (not node)
        (return-from solve1))

      (format t "~a" (node-name node))
      (setf (node-produced node) t))))

(defun solve2
  (nodes
    &key
    (workers-count 5)
    (additional-step-time 60)
    (print-debug nil))

  (let ((workers-nodes (make-list workers-count))
        (steps 0))

    ;; calculate production steps required for each node
    (dolist (node nodes)
      (setf (node-required-production-steps node)
            (+ additional-step-time
               (- (char-int (node-name node)) 64)))
      (setf (node-production-step node) 0))

    ;; produce
    (let ((nodes-to-produce-count (length nodes))
          (produced-nodes-count 0))

      (loop
        (incf steps)

        ; print workers nodes
        (if print-debug
          (progn
            (format t "~%Workers:")
            (dolist (worker-node workers-nodes)
              (format t "  ~A"
                      (if worker-node
                        (format nil "[~A ~2@A/~2A]"
                                (node-name worker-node)
                                (node-production-step worker-node)
                                (node-required-production-steps worker-node))
                        "[       ]")))))

        ; take of produced nodes from workers
        (loop for i from 0 to (1- workers-count) do
              (let ((worker-node (nth i workers-nodes)))
                (if (and worker-node
                         (= (node-production-step worker-node)
                            (node-required-production-steps worker-node)))
                  (progn
                    (setf (node-produced worker-node) t)
                    (format t "~A" (node-name worker-node))
                    (incf produced-nodes-count)
                    (setf (nth i workers-nodes) nil)))))

        (if (= produced-nodes-count
               nodes-to-produce-count)
          (return-from solve2 (1- steps)))

        ; give node to produce to each free worker
        (loop for i from 0 to (1- workers-count) do
              (if (not (nth i workers-nodes))
                (let ((node
                        (car
                          (loop for n in
                                (get-nodes-which-can-be-produced nodes)
                                when (not (find n workers-nodes))
                                collect n))))
                  (setf (nth i workers-nodes) node))))

        ; advance count for each worker
        (dolist (worker-node workers-nodes)
          (if worker-node
            (incf (node-production-step worker-node))))))))


(let ((nodes (parse-input *input*)))
  (solve1 nodes))

(format t "~%")

(let ((nodes (parse-input *input*)))
  (print (solve2 nodes)))
;(solve2 nodes :workers-count 2 :additional-step-time 0))

