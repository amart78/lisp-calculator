(defparameter *op-families*
  '((((#\+) left #'+))
    (((#\-) left #'-))
    (((#\*) left #'*))
    (((#\/) left #'/))
    (((#\^) right #'expt))))

(defun char-is-digit (chr)
  (and (char<= #\0 chr) (char>= #\9 chr)))

(defun str-is-digit (str)
  (reduce (lambda (acc x)
            (and acc (char-is-digit x)))
          str :initial-value t))

(defun find-first-index (lst k)
  (labels ((rec (lst n)
                (cond
                 ((null lst) NIL)
                 ((eq (car lst) k) n)
                 ((and
                   (listp (car lst))
                   (find-first-index (car lst) k)) n)
                 (t (rec (cdr lst) (+ n 1))))))
    (rec lst 0)))

(defun get-priority (sym)
  (find-first-index *op-families* sym))

(defun is-op (chr)
  (get-priority chr))

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
                          ((string= str "") (list #\# str NIL)) ; returns (#\( inner-str)
                          ((str-is-digit str) (list #\# str NIL)) ; returns (#\( inner-str)
                          ((and t (eq (char str 0) #\())
                          (list #\( (extract-parens-op str) NIL)) ; if the first character is a ( so we can extract it
                          (t str))) ;; empty string
      (t (extract-op str split-pos)))))

(defmacro gen-rule (rule)
  (destructuring-bind ((symbol) associativity function) rule :ignore function
    (let ((comparator
           (if (eq associativity 'left)
              '<= ; causes the leftmost recursive solution to override an equivalent solution
             '<)); causes the rightmost recursive solution to override an equivalent solution
          (priority (get-priority symbol)))
      `((eq cur-char ,symbol)
        (if (,comparator (first retval) ,priority)
            retval
          (list ,priority index))))))

#|
evaluation order information
left to right priority tie-breaking <=
recursion unrolls and the earlier equally weighted operator gets returned
right to left priority tie-breaking <
recursion unrolls and if there's a tie, then the later operator gets returned
|#

(defmacro aggregated-rules ()
  (let ((rules (map 'list
          #'(lambda (x) (macroexpand `(gen-rule ,x)))
          (apply #'append *op-families*))))
    `(let ((retval (next-char)))
      (cond
        ((not (is-op cur-char))
          retval) ;; current char is not operator
        ,@rules
        (t retval)))))

(defun find-first-operator (str)
  (defmacro next-char ()
    '(rec (+ 1 index) level strlen))
  (defmacro next-char-up-level ()
    '(rec (+ 1 index) (+ 1 level) strlen))
  (defmacro next-char-down-level ()
    '(rec (+ 1 index) (- 1 level) strlen))

  (labels
      ((rec (index level strlen)
        (if (eq index strlen)
          (list most-positive-fixnum nil)
          (let ((cur-char (char str index)))
            (cond
              ((eq cur-char #\()
                (next-char-up-level)) ;; current char is open paren
              ((eq cur-char #\))
                (next-char-down-level)) ;; current char is close paren
              ((eq level 0)
                (aggregated-rules)) ; some illegal character
              (t (next-char))))))) ;; action cannot be carried out wait for higher priority function
      (second (rec 0 0 (length str)))))

(defun gen-ast (str)
  (labels ((rec (root)
                ;; returns tree of operators '(#\operator '(left subtree) '(right subtree))
                (cond
                 ((eq (first root) #\#) root)
                 (t
                  (list (first root)
                    (if (second root)
                      (rec (extract-highest-op (second root))))
                    (if (third root)
                      (rec (extract-highest-op (third root)))))))))
    (rec (extract-highest-op str))))
(gen-ast "1+-1")


(defun compute-ast (root)
  (cond
   ((eq (first root) #\#)
    (if (string= (second root) "")
      0
      (parse-integer (second root))))
   ((eq (first root) #\+)
    (+ (compute-ast (second root)) (compute-ast (third root))))
   ((eq (first root) #\-)
    (- (compute-ast (second root)) (compute-ast (third root))))
   ((eq (first root) #\*)
    (* (compute-ast (second root)) (compute-ast (third root))))
   ((eq (first root) #\/)
    (/ (compute-ast (second root)) (compute-ast (third root))))
   ((eq (first root) #\^)
    (expt (compute-ast (second root)) (compute-ast (third root))))
   ((eq (first root) #\()
    (compute-ast (second root)))))

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

;; conversion process
;; our convention will be (mainline-level (line) (line) (line))

(defun list-filtered-length (lst)
  (reduce #'(lambda (acc elem)
              (if (symbolp elem) acc (+ acc 1))) lst :initial-value 0))

(defun max-list-length (lst)
  (reduce #'(lambda (acc cur)
              (max (list-filtered-length cur) acc)) lst :initial-value 0))

(defun gen-n-pad (n value)
  (make-list n :initial-element value))

(defun list-pad-block-horizontal (lst &optional (lst-B NIL))
  (let ((max-length (max
                     (max-list-length (cdr lst))
                     (max-list-length (cdr lst-B)))))
    (cons (car lst)
          (map 'list
               #'(lambda (x)
                   (append x (gen-n-pad (- max-length (list-filtered-length x)) #\ )))
               (cdr lst)))))

(defun list-prepad-block-horizontal (lst-A lst-B)
  (let ((max-length 
         (max-list-length (cdr lst-B))))
    (cons (car lst-A)
          (map 'list
               #'(lambda (x)
                   (append (gen-n-pad max-length #\ ) x))
               (cdr lst-A)))))
;;(list-prepad-block-horizontal '(0 (1 2 3 4) (1 2 3 4)) '(0 (1 2) (1 2)))

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
        (cons mainline-A (append lst-A (gen-n-pad num-append NIL))) ; need to append A
      A)))

(defun list-pad-block-vertical (A B)
  ;; only provides the correct number of lines padded to itself
  ;; use multiple-value-bind to catch the new blocks new-A new-B
  (let* ((pre-padded-A (list-vertical-prepend A B))
         (pre-padded-B (list-vertical-prepend B A))
         (new-A (list-vertical-append pre-padded-A pre-padded-B))
         (new-B (list-vertical-append pre-padded-B pre-padded-A)))
    (values
     (list-pad-block-horizontal new-A)
     (list-pad-block-horizontal new-B))))

;; Operator functions
;; blocks are defined as the lists without the mainline in the first position

(defun block-to-list (blk)
  (cons NIL blk))

(defun list-to-block (lst)
  (cdr lst))

(defun block-mass-append (block-A block-B)
  (map 'list #'append block-A block-B))

(defun block-append-at-n (block-A operator n)
  (labels ((
            rec (acc x)
                (cond
                 ((null x) (reverse acc))
                 ((eq n (length acc))
                  (rec
                   (cons (append (car x) (cons operator nil)) acc) ; append the operator to the end of the accumulator list
                   (cdr x)))
                 (t (rec (cons (car x) acc) (cdr x))))))
    (list-to-block
     (list-pad-block-horizontal
      (block-to-list
       (rec NIL block-A))))))

(defun block-surround-at-n (block-A n &optional (prefix NIL) (suffix NIL))
  (labels ((
            rec (acc lines)
                (cond
                 ((null lines) (reverse acc))
                 ((eq n (length acc))
                  (rec
                   (cons (append (cons prefix nil) (car lines) (cons suffix nil)) acc) (cdr lines)))
                 (t
                  (let*
                      ((prefixed-line
                        (if (and prefix (not (symbolp prefix))) ; catch if the symbol is a symbol for highlighting
                            (append
                             (cons #\  NIL)
                             (car lines))
                          (car lines)))
                       (new-line
                        (if (and suffix (not (symbolp suffix))) ; catch if the symbol is a symbol for highlighting
                            (append
                             prefixed-line
                             (cons #\  NIL))
                          prefixed-line)))
                    (rec
                     (cons new-line acc)
                     (cdr lines)))))))
    (list-to-block (list-pad-block-horizontal (block-to-list (rec NIL block-A))))))

(defun conjoin-inline-operator (list-A operator)
  ;; concatenates blocks that use inlined operators
  ;; i.e. ()
  (let* ((mainline
          (car list-A))
         (new-block
          (block-surround-at-n (list-to-block list-A) mainline #\( #\)))
         (aggregate
          (cons mainline new-block)))
    aggregate))

(defun conjoin-horizontal-operator (list-A list-B operator)
  ;; concatenates blocks that utilize horizontal concatenation
  ;; i.e. + - *
  (multiple-value-bind
      (padded-list-A padded-list-B)
      (list-pad-block-vertical list-A list-B)
    (let* ((mainline
            (max (car padded-list-A) (car padded-list-B)))
           (left-block
            (block-append-at-n (list-to-block padded-list-A) operator mainline))
           (right-block
            (list-to-block padded-list-B))
           (aggregate
            (cons mainline (block-mass-append left-block right-block))))
      aggregate)))

(defun conjoin-vertical-operator (list-A list-B operator)
  ;; concatenates blocks that utilize vertical concatenation
  ;; calls out to functions that perform the special concatenations
  ;; i.e. / ^
  (cond
   ((eq operator #\^)
    (let* ((mainline
            (length (list-to-block list-B)))
           (operator-bot-block
            (block-append-at-n (list-to-block list-A) operator (car list-A)))
           (top-block
            (list-to-block (list-prepad-block-horizontal list-B (cons NIL operator-bot-block))))
           (bot-block
            (list-to-block (list-pad-block-horizontal (cons NIL operator-bot-block) (cons NIL top-block))))
           (aggregate
            (cons mainline (append top-block bot-block))))
      aggregate))
   (t
    (let
        ((padded-list-A (list-pad-block-horizontal list-A list-B))
         (padded-list-B (list-pad-block-horizontal list-B list-A)))
      (let* ((mainline
              (car padded-list-A))
             (top-block
              (block-surround-at-n (list-to-block padded-list-A) (- (length (list-to-block padded-list-A)) 1) 'U 'N))
             (bot-block
              (list-to-block padded-list-B))
             (aggregate
              (list-pad-block-horizontal (cons mainline (append top-block bot-block)))))
        aggregate)))))

(defun flavor-format-lst (lst)
  (map 'list #'(lambda (x)
                 (cond
                  ((eq x 'U)
                   (format NIL "~c[4m" #\ESC))
                  ((eq x 'N)
                   (format NIL "~c[0m" #\ESC))
                  (t x)))
       lst))

(defun print-format-lst (fn-block)
  (format t "~%")
  (map 'list #'(lambda (x)
                 (format t "~A~%" (list-to-string (flavor-format-lst x))))
       fn-block)
  NIL)

(defun soln-ast (equation)
  (let* ((ast (gen-ast equation))
         (lst (gen-2d-lst ast))
         (result (compute-ast ast))
         (full-soln (block-append-at-n (list-to-block lst) (format NIL " = ~A [~A]" (float result) result) (car lst))))
    (print-format-lst full-soln)))

(defun gen-2d-lst (root)
  (cond
   ((eq (first root) #\#)
    (list 0 (string-to-list (second root))))
   ((member (first root) '(#\+ #\- #\*))
    (conjoin-horizontal-operator
     (gen-2d-lst (second root))
     (gen-2d-lst (third root))
     (first root)))
   ((member (first root) '(#\/ #\^))
    (conjoin-vertical-operator
     (gen-2d-lst (second root))
     (gen-2d-lst (third root))
     (first root)))
   ((member (first root) '(#\()) ; TODO
    (conjoin-inline-operator
     (gen-2d-lst (second root))
     (first root)))))

(defun main ()
  (soln-ast (read-line))
  (main))
(main)
