;;;; https://adventofcode.com/2018/day/4
(load "common.lisp")

(defvar *example-input*)
(setf *example-input*
"[1518-11-01 00:00] Guard #10 begins shift
[1518-11-01 00:05] falls asleep
[1518-11-01 00:25] wakes up
[1518-11-01 00:30] falls asleep
[1518-11-01 00:55] wakes up
[1518-11-01 23:58] Guard #99 begins shift
[1518-11-02 00:40] falls asleep
[1518-11-02 00:50] wakes up
[1518-11-03 00:05] Guard #10 begins shift
[1518-11-03 00:24] falls asleep
[1518-11-03 00:29] wakes up
[1518-11-04 00:02] Guard #99 begins shift
[1518-11-04 00:36] falls asleep
[1518-11-04 00:46] wakes up
[1518-11-05 00:03] Guard #99 begins shift
[1518-11-05 00:45] falls asleep
[1518-11-05 00:55] wakes up")
(setf *example-input* (split-string-by *example-input* #\Newline :trim-whitespace t))

(defstruct entry
    type
    original-string
    time-string
    time-universal
    guard-id)

(defun parse-string-to-universal-time (time-string)
  (let ((seconds 0)
        (minutes (parse-integer (subseq time-string 14 16)))
        (hours   (parse-integer (subseq time-string 11 13)))
        (day     (parse-integer (subseq time-string  8 10)))
        (month   (parse-integer (subseq time-string  5  7)))
        ; universal time can't handle 1518 year
        ; but year is not important, so we will
        ; use the base 1900
        (year 1900))
    (encode-universal-time seconds minutes hours day month year)))

(defun parse-input-into-entries (input)
  (loop for i in input
        collect

        (let ((input-split (split-string-by i #\] :trim-whitespace t))
              (time-string)
              (note-string)
              (note-split)
              (entry-type))

          (setf time-string (string-left-trim '(#\[) (nth 0 input-split)))
          (setf note-string (nth 1 input-split))
          (setf note-split (split-string-by note-string #\Space))
          (setf entry-type
                (cond
                  ((equal #\G (char note-string 0)) :guard)
                  ((equal #\w (char note-string 0)) :wake-up)
                  ((equal #\f (char note-string 0)) :fall-asleep)
                  (t (progn
                       (format t "ERROR: Unknown type string: ~a~%" (nth 0 note-split))
                       :unknown))))

          (make-entry
            :type entry-type
            :original-string i
            :time-string time-string
            :time-universal (parse-string-to-universal-time time-string)
            :guard-id (if (equal entry-type :guard)
                        (parse-integer (string-left-trim '(#\#) (nth 1 note-split))))))))

(print *example-input*)
(print (parse-input-into-entries *example-input*))

