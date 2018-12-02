;;;; https://adventofcode.com/2018/day/2

(defvar *example-input1* nil)
(setf *example-input1* '("abcdef"
                         "bababc"
                         "abbcde"
                         "abcccd"
                         "aabcdd"
                         "abcdee"
                         "ababab"))

(defvar *example-input2* nil)
(setf *example-input2* '("abcde"
                         "fghij"
                         "klmno"
                         "pqrst"
                         "fguij"
                         "axcye"
                         "wvxyz"))

(defvar *input* nil)
(setf *input* '("wlpiogsvdfecjdqmnxakudrhbz"
                "wbpioesvyfecjuqmlxaktdrhbz"
                "blviogavyfecjuqmnxaktdrhbz"
                "wlpiogsvydecjuqmnipktdrhbz"
                "wlwiogsvyfmcjuqmoxaktdrhbz"
                "wlpiogsvphecjuqmnxaktdrzbz"
                "wlpiogsvyfecjuqmnkakhdrkbz"
                "wlpiogsvyfhcjuqmnxxktddhbz"
                "wlpiogsvyfccfuqmnxgktdrhbz"
                "wlpiogsvhmecjvqmnxaktdrhbz"
                "wlpiogsvyfecjdqqnxaktdrhyz"
                "wlpiogyvycecjaqmnxaktdrhbz"
                "wlpiogsvyfecjfqmnxaktybhbz"
                "wkpiogsvyfmcauqmnxaktdrhbz"
                "wlmiolsvyfecjuqmnxaktdrhbn"
                "wlpioksvyfecjuqmnxaktdrhgs"
                "wlpiogsvyflcjuvmnxsktdrhbz"
                "wgziogsvyfecjuqmnxaktdrhoz"
                "whpingsvyeecjuqmnxaktdrhbz"
                "wlpiogsvyfecjuqgnxaktdvhlz"
                "wlpioasvtfecjuqmnxaktdahbz"
                "wlpihgsvefeceuqmnxaktdrhbz"
                "wlpiogsvyfecyuqwnxaktdghbz"
                "wlpfsgsvyfhcjuqmnxaktdrhbz"
                "wlpiogyvyfecjuqmnxalpdrhbz"
                "wlpiogsvyfescsqmnxaktdrhbz"
                "wluiogsvyfecytqmnxaktdrhbz"
                "wltiorsvyfecjuqmoxaktdrhbz"
                "wlmiogwvyfejjuqmnxaktdrhbz"
                "wlpiogsvyfycjuumnxvktdrhbz"
                "wlkiogsqyfecjqqmnxaktdrhbz"
                "wlpiogsvyfecouqmnxaktfrubz"
                "hupiogsvlfecjuqmnxaktdrhbz"
                "wlpiogsvpxecjuqmnxaksdrhbz"
                "wlpiogsvyfkcjfqmnxxktdrhbz"
                "wlpiogsjyfecjuqnnxakthrhbz"
                "wlpiogsvyfecjuqmnxaktddhdk"
                "wlpipgsvyfecjuqmnhaktdrubz"
                "wlpiogsoyfenjpqmnxaktdrhbz"
                "wlpiogsvyfecnuqmnxaxtdrmbz"
                "wlpiggsvyfjcjuumnxaktdrhbz"
                "wlppogsvyfecjuqmnxautdhhbz"
                "wlpiovbvyfecjuqmnxaktcrhbz"
                "wlpiogsvyfecjoqmnxaktdrobu"
                "wlpiohsvyfecjugmnxakthrhbz"
                "wvpiogovyfecjuqmnxakwdrhbz"
                "wlpiogsbyfecjuqmnxdktdrtbz"
                "wlpnogsvyfecjuqmnxakykrhbz"
                "wlpiogpvyfecjuqmnxvktdrhbs"
                "wlpiogsvkvecjuqmnxadtdrhbz"
                "wlkihgsvyfecjuqmnxlktdrhbz"
                "wlpilgsvyfhcjuqmnxakudrhbz"
                "wlpioksvysgcjuqmnxaktdrhbz"
                "wlpiogsvyfecorqmoxaktdrhbz"
                "wlpiogsvyfectzlmnxaktdrhbz"
                "wlpiogsvywecjuqwnqaktdrhbz"
                "wlpiowsvyfecjuqrnxaftdrhbz"
                "wlpiogsuyfecjutmnxaktnrhbz"
                "wepiowsvyfqcjuqmnxaktdrhbz"
                "wlpirgssyfecjuqmvxaktdrhbz"
                "wlyiogstyfecjuqmnxaktdrhbw"
                "wlpiogseyfecauqmnxaktdjhbz"
                "wlpioisvyfenjuqmnxakrdrhbz"
                "wlpiogsvyfrgjfqmnxaktdrhbz"
                "wlpionsvyfecjmqmjxaktdrhbz"
                "alpiggsvyfecjuqmnxaktdrhkz"
                "wlphngsvyfecjuqmnxfktdrhbz"
                "wlpiogsvyferjuqmlxakttrhbz"
                "wlniogsvefecjuqsnxaktdrhbz"
                "wlpiogsvyfncjurmnxakudrhbz"
                "wlpiogsvyfecjuqmnxaktprlaz"
                "wlpiocsvyfecjupmkxaktdrhbz"
                "wlpihgsvyfecjeqfnxaktdrhbz"
                "wlwioasvyfjcjuqmnxaktdrhbz"
                "wlpifgsvyfecjuqsnxaktdshbz"
                "wlxiogsvyrechuqmnxaktdrhbz"
                "wlpiogovyfxcjuqmnxakkdrhbz"
                "wlpiogsvyfecjkqmdxaktmrhbz"
                "wlpiogsvyfecjwqmntaktdhhbz"
                "wlpiogsvdfecjuqmmxaktbrhbz"
                "wlpiogsvyfecauqmnxaksdrhwz"
                "wlpiogsvwfecjuqznxaktorhbz"
                "wlpiogtvyfecjuqhnxakidrhbz"
                "wlpiogsvyyecjuqmnxaktdrhwt"
                "wljiogsvyfecfuqbnxaktdrhbz"
                "wlpiogsvybecjuqmnxagtdrjbz"
                "wrpiogsvyfecjuqmnuaktdrhbd"
                "wlpiogsvyfecjurmnxnltdrhbz"
                "blpvogsvyaecjuqmnxaktdrhbz"
                "bfpiogyvyfecjuqmnxaktdrhbz"
                "wlpiogsvyfecjuqinxaknddhbz"
                "wlpizgsvvfecjuqxnxaktdrhbz"
                "glpiogsvyrecjuqmnxaktdrhbr"
                "wlpiogskhfecjutmnxaktdrhbz"
                "wlpiogsvyfecmuqmnxaktdribc"
                "wlpioesvwfecjuqmnxakkdrhbz"
                "wlpionsrafecjuqmnxaktdrhbz"
                "wlsiogsvyfecjuqmnaaktdrhvz"
                "bloiogsvyfecjuqmnxakjdrhbz"
                "wlpiogsvyfecjuqenmastdrhbz"
                "wlpiogyvyfecjuqmuxakldrhbz"
                "plpiogovyfecjuvmnxaktdrhbz"
                "wlpiogsvyfebjuqmnkakvdrhbz"
                "wlziogsvyfhcjuqmngaktdrhbz"
                "wlsiogsvyfecjuqmnxaktdrzjz"
                "plbiogsvyfecfuqmnxaktdrhbz"
                "wfpiogsvyfecjuqknxaktdrhiz"
                "wlpiogjbyfecjuqmnxaktprhbz"
                "wmpiogsvyrecjcqmnxaktdrhbz"
                "wlpiogsyyfecjuqmqxaktdrbbz"
                "wlpiogsvyfecjuqknxaetdehbz"
                "wlpiogsvyfezjuqmnxakkdhhbz"
                "wlpiogsvyfecjjqvnxaktdrhiz"
                "wkpiogsvyfucjuqmnxaktdrhbd"
                "lliiogsvyfecjuqmnxaktdrhoz"
                "wlpiogsvyfecjuqmsxdktdshbz"
                "wlprogtvyfecjuqmnxaktvrhbz"
                "wlpizgssyffcjuqmnxaktdrhbz"
                "wlpioasvyfvcjuqmnxakldrhbz"
                "wlpoogsvyyecjuqmnxastdrhbz"
                "wlpiognvyfecjuqmnsaktdrhbr"
                "wlpiogsoyfecjuqmnxaktdrhho"
                "wfpiogsvydecjuqmnxaotdrhbz"
                "wlpiogsvqhecjuqmnxaktdrhhz"
                "wkpiogsvyfeojuqmnxaktdrqbz"
                "wlpiogsvyfeveuqmnxaktdshbz"
                "wlpiogbvyfecjuqmexaktdrcbz"
                "wlpxogsvyfehjsqmnxaktdrhbz"
                "wlpcogsvyfecjuqmnjakttrhbz"
                "wlpiogsvvkecjuqmnxaftdrhbz"
                "wlpiogsvffecnuqmnxaktdnhbz"
                "wlpiogsvyfecjupjnxaktdrhbr"
                "wlpqogsvyfecjuqmnxlktdphbz"
                "wlpxogsvyfecjvqmnxaktirhbz"
                "elpiogsvyfecjuqlnxaqtdrhbz"
                "wspiogsvrfecjuqmnxakadrhbz"
                "wlpiogsmyfecbuqmnxactdrhbz"
                "wlpiogsvyfecauqmnyakzdrhbz"
                "wlsiogsvyfecjuqmnxakvdrnbz"
                "wlpiogsxyfeijuqmnxakndrhbz"
                "wlpiogsvyfecjuumnxakbyrhbz"
                "wlpiogsvyfecjuqmnxhktdyhbo"
                "wlpiogsvyfecjuqqnxnjtdrhbz"
                "wapiogsvyfecjuqmnxoktdrmbz"
                "wlpiogsvyfeejvqmnxaktdrubz"
                "wlpitgsvyeectuqmnxaktdrhbz"
                "alpiogsvyfecjulmnxaktdchbz"
                "wlpiogsvyfecjuqmuxoktdrwbz"
                "wlpiogsvyfzgjuhmnxaktdrhbz"
                "wlpnogsvyfecjuqmdxaktyrhbz"
                "wlpiogsvyfecjuqmnxakthrhra"
                "wliiogsvyfecluqmnxaktdhhbz"
                "wlpiogsvyfecjuymnxaltdrhwz"
                "wlpiogsvyfeljuqmnxaktyrhbd"
                "wlpiygsvvfecjuqmfxaktdrhbz"
                "wlpiogihsfecjuqmnxaktdrhbz"
                "wlpiogjvyfecjuqmnhuktdrhbz"
                "wldiogsvyfecjiqmwxaktdrhbz"
                "wlpiogsvjfecjuqmnxaktdrgbr"
                "wlpioisvyfecjuqwnxabtdrhbz"
                "wlviogsvyfscjuqmnxqktdrhbz"
                "wlpiogsvyfecjuqmuxakbdrubz"
                "wlpiogsvyfecjuqmnxmatdrhqz"
                "wlpiogsvyfbcjuqwmxaktdrhbz"
                "wlpiogsvyfexjuqmnxuxtdrhbz"
                "wljiogsvbfecjuqmnxartdrhbz"
                "wlpvogsvyfeujuqmnxaktdmhbz"
                "wnpiogsvyfekjuqanxaktdrhbz"
                "wlprogsvyfecjuqmzxvktdrhbz"
                "wkpiogvvyfecjuqmnxaktdrabz"
                "wlpiogsvwfecjuqmnxaktkbhbz"
                "wlpiogsvyfecjlqmnxtttdrhbz"
                "wlpioqsvyfecjuqznxaktyrhbz"
                "wlpiogsvyfecjuqmnxnethrhbz"
                "wlpiogsyyfgcjuqmnxaktdrhbm"
                "wlpiopsvbfecjuqmnxaktdlhbz"
                "wloqogsvyfucjuqmnxaktdrhbz"
                "wlpiogsvmfecjuqmnxmktdrhtz"
                "wlhiogsvyfecjuhmnxaktsrhbz"
                "wlpioggvpfecjufmnxaktdrhbz"
                "wlpiogsvyfbcjuomnxaktdrhbh"
                "wlpmogsvyfecyuqmnxoktdrhbz"
                "wlpiogslyfecjuqmnxaptyrhbz"
                "tlpiogsnyfecguqmnxaktdrhbz"
                "wlpiogsvyfecjuqmnxwktwehbz"
                "wlpiogsvgfecjuqmnxaktdrabu"
                "wbpiogsvyfecjuqmnxagtdrhoz"
                "wlwipgsvyfecjuqmnxaktdrhbu"
                "wlpwogsvykeczuqmnxaktdrhbz"
                "wlpgogsvwfecjuqmnxcktdrhbz"
                "wlpiogsqyfecjuqmrxaktdrrbz"
                "wlpiogsvyfecjuqmnxakthrafz"
                "wypicgseyfecjuqmnxaktdrhbz"
                "wlpiogcvqfecjuqmnxaktdrhzz"
                "wlriogsvyfecouqmnkaktdrhbz"
                "wlpiogsvyfemjulmnxaktdrhdz"
                "flpiogadyfecjuqmnxaktdrhbz"
                "wupiogsvyfbvjuqmnxaktdrhbz"
                "wlpiogsvyfebjummnxaktdrrbz"
                "wjpiogsvyfecjuqmnxaktprybz"
                "wlpirgsvyfecjiqmnxaatdrhbz"
                "bvpiogsvyfecjuqmnxaktdrhez"
                "wlpiogsvyfxcjuqmnxykzdrhbz"
                "wlkiwgsqyfecjqqmnxaktdrhbz"
                "wepaogsvyfecjxqmnxaktdrhbz"
                "wlpiovsvyfecjjqmnxaktdmhbz"
                "wlpioysryfecjuqmnxaktdrhiz"
                "wlpizjsvyfecjuvmnxaktdrhbz"
                "dlpiogsvyfecjucmnxakbdrhbz"
                "wlpiogsccfecjrqmnxaktdrhbz"
                "wlpioggvyfecpuqmnxagtdrhbz"
                "wlpiogsvyfvcjuumlxaktdrhbz"
                "wwpiogsryfjcjuqmnxaktdrhbz"
                "wlpiogsvyfecjuqynxaktdrogz"
                "wlpiogsvyfecjpqmnxskbdrhbz"
                "wlpiogsvyfecjuhmfxaktvrhbz"
                "wlpiogevyfecjrqmnwaktdrhbz"
                "wlpiigsvyfemjuqmnxaktdrhtz"
                "wlpcogsvyfecjuqhnxakgdrhbz"
                "wupiogsvyfxcjuqmnxaktdrhgz"
                "wlsiogsvyfecjuqenxuktdrhbz"
                "wlpioglvyfecjujmexaktdrhbz"
                "wlriogsvyfeljuqmnxattdrhbz"
                "wlpiogsvyfecfuqmhxaktkrhbz"
                "wlppogsvyfecjuqmxxabtdrhbz"
                "wlniogsvyfevjuqwnxaktdrhbz"
                "wlhiogsvyfecjuqmnxactxrhbz"
                "ilpiogivyflcjuqmnxaktdrhbz"
                "wlpmogsvyfecjuqmnxaktdrlbs"
                "wipiogsvyfeqjuqmnxaktrrhbz"
                "wvpiogsvyfecjuqknxaktdrrbz"
                "wwpioguvyfecxuqmnxaktdrhbz"
                "wlpiogsvkfdcjuqmnxaktdzhbz"
                "wlpiogfvyfecjuqmnxadtdrhbg"
                "wlpiogsvyzefjuqfnxaktdrhbz"
                "wlpiogstyfechuqmnxaktdchbz"
                "wlpiogszyfedjuqmnxsktdrhbz"
                "wzpiozsvyfncjuqmnxaktdrhbz"
                "xlpiogsvyfefjuqmnmaktdrhbz"
                "wlpiogsvyfebxummnxaktdrhbz"
                "wlpiogsgyfecfurmnxaktdrhbz"
                "wlpqogsvyfecjuomnxaktdrhbi"
                "wlpiogjvufecjuqmnxaktdrhbd"
                "wlpiolsvyfecduqmnxaktrrhbz"
                "wlpxogsvyfecjuqmnxaktgrhbk"
                "wlpiogsfyfncjuqmnxsktdrhbz"
                "wlpioggvyfecjufmnxaktdrebz"
                "wlpiogsvyfecfujmnxaktdrwbz"
                "rlpiogsvyfecjlqmnxaktdqhbz"
                "wlpfogsvyfecjuimnxaktfrhbz"))

(defun print-hash-table (table)
  (maphash
    #'(lambda (key value) (format t "~S: ~S~%" key value))
    table))

(defun count-chars (text)
  (let ((chars-count-hash-table (make-hash-table)))
    (loop for c across text do
          (if (not (gethash c chars-count-hash-table))
            (setf (gethash c chars-count-hash-table) 0))
          (setf
            (gethash c chars-count-hash-table)
            (+ (gethash c chars-count-hash-table) 1)))
    chars-count-hash-table))

(defun has-n-chars (text n)
  (let ((chars-count (count-chars text)))
    (loop for c being the hash-value of chars-count do
          (if (= c n) (return t)))))

(defun count-diff-chars (left right)
  (let ((l-length (length left))
        (r-length (length right))
        (common-length 0)
        (diff 0))
    (setf diff (abs (- l-length r-length)))
    (setf common-length (min l-length r-length))
    (decf common-length)
    (loop for i upto common-length do
          (if (not (equal (char left i) (char right i)))
            (incf diff)))
    diff))

(defun get-string-with-common-chars (left right)
  (let ((common-chars (list))
        (l-length (length left))
        (r-length (length right))
        (common-length 0))
    (setf common-length (min l-length r-length))
    (decf common-length)
    (loop for i upto common-length do
          (if (equal (char left i) (char right i))
            (push (char left i) common-chars)))
    (coerce (reverse common-chars) 'string)))

(defun solve1 (input)
  (let ((twosCount 0) (threesCount 0))
    (dolist (i input)
      (if (has-n-chars i 2) (incf twosCount))
      (if (has-n-chars i 3) (incf threesCount)))
    (* twosCount threesCount)))

(defun solve2 (input)
  (dolist (l input)
    (dolist (r input)
      (if (= (count-diff-chars l r) 1)
        (return-from solve2 (get-string-with-common-chars l r))))))

(print (solve1 *input*))
(print (solve2 *input*))

