(setq *symbols* '(#\+ #\- #\* #\/))
(defun gen-ast (str)
  (cond
   ((eql "" str) nil)
   ((is-digit str) (str-to-num str))
   (t (let ((op-l-r-list (split-at-first-operator str)))
        (let
            ((op (first op-l-r-list))
             (left (second op-l-r-list))
             (right (third op-l-r-list)))
          '(op (gen-ast left) (gen-ast right)))))))

(defun find-first-operator (str)
  (defmacro next-char ()
    '(rec (+ 1 index) level strlen))
  (defmacro next-char-up-level ()
    '(rec (+ 1 index) (+ 1 level) strlen))
  (defmacro next-char-down-level ()
    '(rec (+ 1 index) (- 1 level) strlen))
  (labels ((rec (index level strlen)
                (cond
                 ((eq index strlen)
                  (list 99 nil)) ; TODO make more elegant value to return
                 ((eq (char str index) #\))
                  (next-char-up-level)) ;; current char is open paren
                 ((eq (char str index) #\()
                  (next-char-down-level)) ;; current char is close paren
                 ((eq level 0)
                      (cond
                       ((null (member (char str index) '(#\+ #\- #\* #\/)))
                        (next-char)) ;; current char is not operator
                       ((member (char str index) '(#\+ #\-))
                        (list 0 index)) ;; current char is an additive character terminate early (priority index)
                       ((member (char str index) '(#\*))
                        (let ((retval (next-char)))
                          (print retval)
                          (if (< (first retval) 1)
                              retval
                              (list 1 index))))
                       ((member (char str index) '(#\/))
                        (let ((retval (next-char)))
                          (print retval)
                          (if (< (first retval) 2)
                              retval
                            (list 2 index))))
                       ((member (char str index) '(#\^))
                        (let ((retval (next-char)))
                          (print retval)
                          (if (< (first retval) 3)
                              retval
                            (list 3 index))))
                       (t (next-char)))) ; unnecessary?
                 ;; current char is a multiplicative character if all else fails, return this
                 (t (next-char))))) ;; action cannot be carried out wait for higher priority function
    (second (rec 0 0 (length str)))))

(find-first-operator "1/(1+1+2)*2*2*2*2")
(defmacro extract-parens-op (str)
  (subseq 1 (- (length str) 1 )))

(defun split-string (str midpoint)
  (subseq str midpoint) (subseq str midpoint (length str)))

(defun extract-op (str midpoint)
  (list (char str midpoint) (subseq str 0 midpoint) (subseq str (+ 1 midpoint))))

(defun extract-highest-op (str)
  (print str)
  (let ((split-pos (find-first-operator str)))
    (cond
     ((null split-pos) (cond
                        ((eq (char str 0) #\() (list #\( (extract-parens-op str) "")) ; if the first character is a ( so we can extract it
                        ((str-is-digit str) (list #\# str "")) ; returns (#\( inner-str)
                        (t str))) ;; empty string
     (t (extract-op str split-pos)))))
;; returns (operator "left string" "right string")

(defun recursive-extractor (str)
  (labels (
           (rec (root)
                (cond
                 ((eq (first root) #\#) root)
                 (t
                  (list (first root)
                        (rec (extract-highest-op (second root)))
                        (rec (extract-highest-op (third root))))))))
    (rec (extract-highest-op str))))
;; returns tree of operators '(#\operator '(left subtree) '(right subtree))

(recursive-extractor "1/(1+2/2)*1*1")

(split-string "(1/1*1+1)")

(defmacro char-is-digit (chr)
  `(and (char<= #\0 ,chr) (char>= #\9 ,chr)))

(defun str-is-digit (str)
  (reduce (lambda (acc x)
            (and acc (char-is-digit x)))
          str :initial-value t))

#|
;; testing char-is-digit
(and
 (char-is-digit #\0)
 (char-is-digit #\1)
 (char-is-digit #\2)
 (char-is-digit #\3)
 (char-is-digit #\4)
 (char-is-digit #\5)
 (char-is-digit #\6)
 (char-is-digit #\7)
 (char-is-digit #\8)
 (char-is-digit #\9))
;; testing str-is-digit
(str-is-digit "12345")
(and
 (str-is-digit "1+1")
 (str-is-digit "deadbeef"))

(extract-highest-op "1/1*1*1")
(extract-highest-op "1/1")
(extract-highest-op "1")
(find-first-operator "1/1*1+1")
#|
|#
