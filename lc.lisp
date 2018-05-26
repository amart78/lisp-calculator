#!/usr/bin/clisp
;; entries ((symbol) associativity argc priority function)
(defparameter *op-families*
  '((((#\+) left 2 0 #'+))
    (((#\-) left 2 1 #'-))
    (((#\*) left 2 2 #'*))
    (((#\%) left 2 3 #'mod))
    (((#\/) left 2 3 #'/))
    (((#\-) right 1 4 #'-))
    (((#\^) right 2 5 #'expt))
    (((#\() left 1 6 #'apply))))

(defun char-is-digit (chr)
  (not (is-op chr)))

(defun str-is-digit (str)
  (not (find-if (lambda (x) (not (char-is-digit x))) str)))

(defun find-first-index (lst k)
  (labels ((rec (lst n)
                (cond
                  ((null lst) NIL)
                  ((eq (car lst) k) n)
                  ((and
                     (listp (car lst))
                     (find-first-index (car lst) k))
                      n)
                  (t (rec (cdr lst) (+ n 1))))))
    (rec lst 0)))

(defun is-op (sym)
  (find-first-index *op-families* sym))

(defun detected-unary-operator (str index)
  (or (eq index 0) (is-op (char str (- index 1)))))

(defun detected-binary-operator (str index)
  (not (detected-unary-operator str index)))

(defmacro gen-cond-rule (rule)
  (destructuring-bind ((symbol) associativity argc priority function) rule :ignore function
    (let ((comparator
            (if (eq associativity 'left)
              '> ; causes the leftmost recursive solution to override an equivalent solution
              '>=)); causes the rightmost recursive solution to override an equivalent solution
          (arg-type
            (if (eq argc 1)
              '(detected-unary-operator str index)
              '(detected-binary-operator str index))))
      `((and
          (eq cur-char ,symbol) ; is this a matching symbol
          (,@arg-type) ; does this have the right number of arguments
          (,comparator (first retval) ,priority)) ; does this have 
        (list ,priority index)))))

(defmacro aggregated-rules ()
  (let ((rules 
          (map 'list
               #'(lambda (x) (macroexpand `(gen-cond-rule ,x)))
               (apply #'append *op-families*))))
    `(let ((retval (next-char)))
       (cond
         ((not (is-op cur-char))
          retval) ;; current char is not operator
         ,@rules
         (t retval)))))

(defun find-first-operator (str)
  (macrolet ((next-char ()
    '(rec (+ 1 index) level strlen))
  (next-char-up-level ()
    '(rec (+ 1 index) (+ 1 level) strlen))
  (next-char-down-level ()
    '(rec (+ 1 index) (- 1 level) strlen)))
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
      (second (rec 0 0 (length str))))))

(defun gen-ast (str)
  (labels ((rec (root)
    (destructuring-bind (op arg1 arg2) root
      ;; returns tree of operators '(#\operator '(left subtree) '(right subtree))
      (case op
        (#\# root)
        (otherwise
          (list op
            (if arg1 (rec (extract-highest-op arg1)))
            (if arg2 (rec (extract-highest-op arg2)))))))))
    (rec (extract-highest-op str))))

(defun extract-highest-op (str)
  ;; returns (operator "left string" "right string")
  (labels (
  (extract-parens-op (str)
    (subseq str 1 (- (length str) 1 )))
  (extract-op (str midpoint)
    (list (char str midpoint) (subseq str 0 midpoint) (subseq str (+ 1 midpoint)))))
  (let ((split-pos (find-first-operator str)))
    (cond
      ((null split-pos) (cond
                          ((string= str "") (list #\# str NIL)) ; returns (#\( inner-str)
                          ((str-is-digit str) (list #\# str NIL)) ; returns (#\( inner-str)
                          ((and t (eq (char str 0) #\())
                           (list #\( (extract-parens-op str) NIL)) ; if the first character is a ( so we can extract it
                          (t str))) ;; empty string
      (t (extract-op str split-pos))))))

(defun parse-value (str)
  ;; parses strings(decimal or not) into values
   (if (position #\. str)
      (multiple-value-bind (new-str magnitude)
         (let ((split-pos (position #\. str)))
            (values
              (format nil "~A~A"
                      (subseq str 0 split-pos)
                      (subseq str (+ 1 split-pos)))
              (- (length str) (+ split-pos 1))))
         (float (/ (parse-integer new-str) (expt 10 magnitude))))
      (parse-integer str)))

(defmacro aggregated-eval ()
  ;; takes all of the operators and puts them into a list to be matched with
  ;; the current operator symbol and uses the given evaluation rule
  (let ((rules 
          (map 'list
               #'(lambda (x) (macroexpand `(gen-eval-rule ,x)))
               (apply #'append *op-families*))))
    `(case op
      (#\# (if (string= arg1 "")
               0
               (parse-value arg1)))
      ,@rules)))

(defmacro gen-eval-rule (rule)
  ;; generates the matching condition for an operation to be applied
  (destructuring-bind ((sym) associativity argc priority func) rule :ignore func
    (case argc
      (1 `(,sym (compute-ast arg1)))
      (2 `(,sym (funcall ,func (compute-ast arg1) (compute-ast arg2)))))))

(defun compute-ast (root)
  (destructuring-bind (op arg1 arg2) root
      (aggregated-eval)))

(defun rebuild-eq (root)
  (destructuring-bind (op arg1 arg2) root
    (cond
      ((null root))
      ((eq op #\#)
        (format t "~A" arg1))
      ((eq op #\()
        (format t "(")
      (rebuild-eq arg1)
        (format t ")"))
      (t
        (rebuild-eq arg1)
        (format t "~A" op)
        (rebuild-eq arg2)))))

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
  (block-surround-at-n block-A n NIL operator))

(defun block-surround-at-n (block-A n &optional (prefix NIL) (suffix NIL))
  (labels ((
            surround-line (acc lines)
            (cond
              ((null lines) (reverse acc))
              ((eq n (length acc))
               (let*
                 ((prefixed-line
                    (if prefix ; catch if the symbol is a symbol for highlighting
                      (append (cons prefix NIL) (car lines))
                      (car lines)))
                  (new-line
                    (if suffix ; catch if the symbol is a symbol for highlighting
                      (append prefixed-line (cons suffix NIL))
                      prefixed-line)))
                 (surround-line
                   (cons new-line acc)
                   (cdr lines))))
              (t
                (let*
                  ((prefixed-line
                     (if (and prefix (not (symbolp prefix))) ; catch if the symbol is a symbol for highlighting
                       (append (cons #\  NIL) (car lines))
                       (car lines)))
                   (new-line
                     (if (and suffix (not (symbolp suffix))) ; catch if the symbol is a symbol for highlighting
                       (append prefixed-line (cons #\  NIL))
                       prefixed-line)))
                  (surround-line
                    (cons new-line acc)
                    (cdr lines)))))))
    (list-to-block (list-pad-block-horizontal (block-to-list (surround-line NIL block-A))))))

(defun conjoin-inline-operator (list-A operator) :ignore operator
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
              (list-to-block (list-prepad-block-horizontal list-B (block-to-list operator-bot-block))))
            (bot-block
              (list-to-block (list-pad-block-horizontal (block-to-list operator-bot-block) (block-to-list top-block))))
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
                 (case x
                   (U (format NIL "~c[4m" #\ESC))
                   (N (format NIL "~c[0m" #\ESC))
                   (otherwise x))) lst))

(defun print-format-lst (fn-block)
  (map 'list #'(lambda (x)
                 (format t "~A~%" (list-to-string (flavor-format-lst x))))
       fn-block))

(defun soln-ast (equation)
  (let* ((ast (gen-ast equation))
         (lst (gen-2d-lst ast))
         (result (compute-ast ast))
         (result-string (format NIL " = ~A [~A]" (float result) result))
         (output-length (+ (length result-string) (list-filtered-length (nth (car lst) (list-to-block lst)))))
         (full-soln 
           (block-append-at-n (list-to-block lst) result-string (car lst))))
    (format t "~v@{~A~:*~}~%~%" output-length "_")
    (print-format-lst full-soln)
    (format t "~v@{~A~:*~}~%" output-length "_")))

(defun gen-2d-lst (root)
  (destructuring-bind (op arg1 arg2) root
    (case op
      (#\#
       (list 0 (string-to-list (second root))))
      ((#\+ #\- #\* #\%)
       (conjoin-horizontal-operator (gen-2d-lst arg1) (gen-2d-lst arg2) op))
      ((#\/ #\^)
       (conjoin-vertical-operator (gen-2d-lst arg1) (gen-2d-lst arg2) op))
      ((#\() (conjoin-inline-operator (gen-2d-lst arg1) op)))))

(defun main ()
  (format t "lc> ")
  (soln-ast (read-line))
  (main))
(main)
