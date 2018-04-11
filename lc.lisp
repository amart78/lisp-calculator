(defun is-op (chr)
  (member chr '(#\+ #\- #\* #\/ #\^)))


(defun extract-highest-op (str)
  ;; returns (operator "left string" "right string")
  (defun extract-parens-op (str)
    (subseq str 1 (- (length str) 1 )))
  (defun split-string (str midpoint)
    (subseq str midpoint) (subseq str midpoint (length str)))
  (defun extract-op (str midpoint)
    (list (char str midpoint) (subseq str 0 midpoint) (subseq str (+ 1 midpoint))))
  (let ((split-pos (find-first-operator str)))
    (cond
     ((null split-pos) (cond
                        ((and t (eq (char str 0) #\())
                         (list #\( (extract-parens-op str) NIL)) ; if the first character is a ( so we can extract it
                        ((str-is-digit str) (list #\# str NIL)) ; returns (#\( inner-str)
                        (t str))) ;; empty string
     (t (extract-op str split-pos)))))

  (defun find-first-operator (str)
    (defmacro next-char ()
      '(rec (+ 1 index) level strlen))
    (defmacro next-char-up-level ()
      '(rec (+ 1 index) (+ 1 level) strlen))
    (defmacro next-char-down-level ()
      '(rec (+ 1 index) (- 1 level) strlen))
    (labels ((rec (index level strlen)
                  (if (eq index strlen)
                      (list 99 nil)
                    (let ((cur-char (char str index)))
                      (cond
                       ((eq cur-char #\()
                        (next-char-up-level)) ;; current char is open paren
                       ((eq cur-char #\))
                        (next-char-down-level)) ;; current char is close paren
                       ((eq level 0)
                        (let ((retval (next-char)))
                          (cond
                           ((not (is-op cur-char))
                            retval) ;; current char is not operator
                           ((member cur-char '(#\+ #\-))
                            (if (< (first retval) 0)
                                retval
                              (list 0 index)))
                           ((member cur-char '(#\*))
                            (if (< (first retval) 1)
                                retval
                              (list 1 index)))
                           ((member cur-char '(#\/))
                            (if (< (first retval) 2)
                                retval
                              (list 2 index)))
                           ((member cur-char '(#\^))
                            (if (< (first retval) 3)
                                retval
                              (list 3 index)))
                           (t retval)))) ; some illegal character
                       (t (next-char))))))) ;; action cannot be carried out wait for higher priority function
      (second (rec 0 0 (length str)))))

(defun gen-ast (str)
  (labels ((
            ;; returns tree of operators '(#\operator '(left subtree) '(right subtree))
            rec (root)
                (cond
                 ((eq (first root) #\#) root)
                 (t
                  (list (first root)
                        (if (second root)
                            (rec (extract-highest-op (second root))))
                        (if (third root)
                            (rec (extract-highest-op (third root)))))))))
    (rec (extract-highest-op str))))


(defun compute-bst (root)
  (cond
   ((eq (first root) #\#)
    (parse-integer (second root)))
   ((eq (first root) #\+)
    (+ (compute-bst (second root)) (compute-bst (third root))))
   ((eq (first root) #\-)
    (- (compute-bst (second root)) (compute-bst (third root))))
   ((eq (first root) #\*)
    (* (compute-bst (second root)) (compute-bst (third root))))
   ((eq (first root) #\/)
    (/ (compute-bst (second root)) (compute-bst (third root))))
   ((eq (first root) #\^)
    (expt (compute-bst (second root)) (compute-bst (third root))))
   ((eq (first root) #\()
    (compute-bst (second root)))))

(defun rebuild-eq (root)
  (cond
   ((null root))
   ((eq (first root) #\#)
    (format t "~A" (second root)))
   ((eq (first root) #\()
    (format t "(")
    (rebuild-eq (second root))
    (format t ")"))
   (t
    (rebuild-eq (second root))
    (format t "~A" (first root))
    (rebuild-eq (third root)))))

(defun char-is-digit (chr)
  (and (char<= #\0 chr) (char>= #\9 chr)))

(defun str-is-digit (str)
  (reduce (lambda (acc x)
            (and acc (char-is-digit x)))
          str :initial-value t))

;; conversion process
;; our convention will be (mainline-level (line) (line) (line))

(defun list-filtered-length (lst)
  (reduce #'(lambda (acc elem)
              (if (symbolp elem) acc (+ acc 1))) lst :initial-value 0))

(list-filtered-length '(3 6 9 12 k))

(defun max-list-length (lst)
  (reduce #'(lambda (acc cur)
              (max (list-filtered-length cur) acc)) lst :initial-value 0))

(max-list-length '((1 2) (2) (1) (1 2 3 4)))

(defun gen-n-pad (n value)
  (make-list n :initial-element value))

(defun list-pad-block-horizontal (lst &optional (lst-B NIL))
  (let ((max-length (max
         (max-list-length (cdr lst))
         (max-list-length (cdr lst-B)))))
    (cons (car lst)
          (map 'list
               #'(lambda (x)
                   (append x (gen-n-pad (- max-length (list-filtered-length x)) 0)))
               (cdr lst)))))

(defun list-to-string (lst)
  (format nil "~{~A~}" lst))

(defun string-to-list (str)
  (coerce str 'list))

(defun list-get-number-vertical-prepend (A B)
  ;; prepend A by B's excess rows
  ;; if positive A requires prepend
  ;; else B requires prepend
  (- (first B) (first A)))

(defun list-vertical-prepend (A B)
  ;; prepend A by B
  ;; will cause the mainline to shift
  (let ((num-prepend (list-get-number-vertical-prepend A B))
        (mainline-A (car A))
        (lst-A (cdr A)))
    (if (> num-prepend 0)
        (cons
         (+ mainline-A num-prepend)
         (append (gen-n-pad num-prepend NIL) lst-A)) ; need to prepend A
      A)))

(defun list-vertical-append (A B)
  ;; must be invoked after prepend since it's relying on
  ;; the guarantee that the remaining length difference
  ;; is due to missing lines at the end
  (let ((num-append (- (length B) (length A)))
        (mainline-A (car A))
        (lst-A (cdr A)))
    (if (> num-append 0)
        (cons mainline-A (append (gen-n-pad num-append NIL) lst-A)) ; need to prepend A
      A)))

(defun list-pad-block-vertical (A B)
  ;; use multiple-value-bind to catch the new blocks new-A new-B
  (let* ((pre-padded-A (list-vertical-prepend A B))
        (pre-padded-B (list-vertical-prepend B A))
        (new-A (list-vertical-append pre-padded-A pre-padded-B))
        (new-B (list-vertical-append pre-padded-B pre-padded-A))
        )
    (values
      (list-pad-block-horizontal new-A new-B)
      (list-pad-block-horizontal new-B new-A))))

;; Operator functions
;; blocks are defined as the lists without the mainline in the first position

(defun block-mass-append (block-A block-B)
  (map 'list #'append block-A block-B))

(defun block-append-at-n (block-A operator n)
  (labels ((
      rec (acc x)
          (cond
           ((null x) (reverse acc))
           ((eq n (length acc))
            (rec (cons (append (car x) (cons operator nil)) acc) (cdr x)))
           (t (rec (cons (car x) acc) (cdr x))))))
    (cdr (list-pad-block-horizontal (cons NIL (rec NIL block-A))))))

(defun block-surround-at-n (block-A prefix suffix n)
  (labels ((
            rec (acc x)
                (cond
                 ((null x) (reverse acc))
                 ((eq n (length acc))
                  (rec
                   (cons (append (cons prefix nil) (car x) (cons suffix nil)) acc) (cdr x)))
                 (t (rec (cons (car x) acc) (cdr x))))))
    (cdr (list-pad-block-horizontal (cons NIL (rec NIL block-A))))))

(defun conjoin-inline-operator (list-A list-B operator)
  ;; concatenates blocks that use inlined operators
  ;; i.e. ()
  )

(defun conjoin-horizontal-operator (list-A list-B operator)
  ;; concatenates blocks that utilize horizontal concatenation
  ;; i.e. + - *
  (multiple-value-bind
      (padded-list-A padded-list-B)
      (list-pad-block-vertical list-A list-B)
    (let* ((mainline
              (car padded-list-A))
           (left-block
              (block-append-at-n (cdr padded-list-A) operator mainline))
           (right-block
              (cdr padded-list-B)))
      (cons mainline (block-mass-append left-block right-block)))))

(defun conjoin-vertical-operator (list-A list-B operator)
  ;; concatenates blocks that utilize vertical concatenation
  ;; calls out to functions that perform the special concatenations
  ;; i.e. / ^
  (cond
   ((eq operator #\^) NIL) ; TODO
   (t
    (let
        ((padded-list-A (list-pad-block-horizontal list-A list-B))
         (padded-list-B (list-pad-block-horizontal list-B list-A)))
      (let* ((mainline
              (car padded-list-A))
             (top-block
              (block-surround-at-n (cdr padded-list-A) 'U 'N mainline))
             (bot-block
              (cdr padded-list-B)))
        (cons
         mainline
         (append top-block bot-block)))))) ; it would be stupid if somehow the mainline wasn't the bottom of the top
  )

(gen-ast "1/(2/3*4+5)+6")

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
(gen-ast "1/(1+2/2)*1*1")

(split-string "(1/1*1+1)")

(find-first-operator "1/(1+1+2)*2*2*2*2")
(gen-ast "1/(2/3*4+5)+6")
(rebuild-eq (gen-ast "1/(2/3*4+5)+6"))
(rebuild-eq (gen-ast "(1/1)+2/3*4+5+6"))
(rebuild-eq (gen-ast "1/6"))
(compute-bst (gen-ast "1/6+1"))
(compute-bst (gen-ast "6/6+1"))
(compute-bst (gen-ast "(1/1)+2/3*4+5+6"))

(gen-n-pad 10 '())
(list-pad-block-length '(1 (1 2) (1 2) (1 2 3 4)))
(list-vertical-prepend '(1 (1 2) (1 2) (1 2 3 4)) '(3 (1 2) (1) (1) (1)))

(list-pad-block-vertical '(1 (1 2) (1 2) (1 2 3 4)) '(3 (1 2) (1) (1) (1)))

(block-mass-append (list-pad-block-vertical '(3 (0 0 0 0) (0 0 0 0) (1 2 0 0) (1 2 0 0) (1 2 3 4)) '(3 (0 0) (1 2) (1 0) (1 0) (1 0))))
(block-mass-append '((0 0 0 0) (0 0 0 0) (1 2 0 0) (1 2 0 0) (1 2 3 4)) '((0 0 0 0) (1 2 0 0) (1 0 0 0) (1 0 0 0) (1 0 0 0)))
(block-append-at-n '((0 0 0 0) (0 0 0 0) (1 2 0 0) (1 2 0 0) (1 2 3 4)) #\k 3)
(conjoin-vertical-operator '(1 (1 2) (1 2) (1 2 3 4)) '(3 (1 2) (1) (1) (1)) #\/)
(conjoin-horizontal-operator '(1 (1 2) (1 2) (1 2 3 4)) '(3 (1 2) (1) (1) (1)) #\+)
#|
|#
