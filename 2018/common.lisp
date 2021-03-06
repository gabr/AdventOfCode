(defun print-hash-table (table &key (separator #\Newline))
  (maphash
    #'(lambda (key value) (format t "~S: ~S~a" key value separator))
    table))

(defun print-hash-hash-table (table)
  (maphash
    #'(lambda (key value)
        (format t "~S: [" key)
        (print-hash-table value :separator ", ")
        (format t "]~%"))
    table))

(defun string-trim-all-whitespace-characters (string-to-trim)
  (string-trim
    '(#\Space #\Newline #\Backspace #\Tab #\Linefeed #\Page #\Return #\Rubout)
    string-to-trim))

(defun split-string-by (string-to-split by-char &key (trim-whitespace nil))
  "Returns a list of substrings of string
  divided by ONE given 'by character' each.
  Note: Two consecutive 'by characters' will be seen
  as if there were an empty string between them."
  (loop for i = 0 then (1+ j)
        as j = (position by-char string-to-split :start i)
        collect (let ((split (subseq string-to-split i j)))
                  (if trim-whitespace
                    (string-trim-all-whitespace-characters split)
                    split))
        while j))

(defun strings-list-to-numbers-list (strings)
  (loop for s in strings
        collect (parse-integer s)))

(defun string-numbers-to-numbers-list (input-string &key (delimiter #\,))
  (strings-list-to-numbers-list
    (split-string-by input-string delimiter)))

