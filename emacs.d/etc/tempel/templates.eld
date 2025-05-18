;; ~/.config/emacs/templates

lisp-mode

(defpar "(defparameter " p "\n  \"" p "\")")
(defvar "(defvar " p "\n  \"" p "\")")
(defmacro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
(defun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
(cond "(cond" n "(" q "))" >)
(let "(let (" p ")" n> r> ")")
(loop "(loop for " p " in " p " do" n> r> ")")
(stacksize
 "#-swank
(unless (member :child-sbcl *features*)
  ;; Relaunch with increased control stack size
  (quit :recklessly-p t
        :unix-status
        (process-exit-code
         (run-program
          *runtime-pathname*
          `("--control-stack-size" "256MB"
            "--noinform" "--disable-ldb"
            "--lose-on-corruption" "--end-runtime-options"
            "--eval" "(push :child-sbcl *features*)"
            "--script" ,(namestring *load-pathname*))
          :output t :error t :input t))))" n)
(split-string
 "(defun split-string (string &rest parser-fns)
  \"split string by spaces. then, apply the elements to functions. if there are more elements than functions, the remaining elements are applied to the last function. if no functions are provided, the elements will be string\"
  (loop with str = string for p = (position #\\Space str)
        for fn = (or (pop parser-fns) fn)
        collect (funcall (or fn #'identity) (subseq str 0 p))
        while p
        do (setf str (subseq str (1+ p)))))" n)
(type-object
 "(defun type-object (obj)
  (cond ((listp obj) 'list)
        ((stringp obj) 'string)
        ((vectorp obj) 'vector)
        (t 'unkonwn)))" n)
(range
 "(defmacro range (start end &key (by 1))
  `(loop for i ,(if (<= start end) 'from 'downfrom) ,start to ,end by ,by
         collect i))" n)
(crosspair
 "(defun crosspair (lst1 lst2)
  (loop for x in lst1
        append (loop for y in lst2
                     collect (cons x y))))" n)
(dirpair
 "(defun dirpair (&optional (below 9))
  (destructuring-bind (r l u d -) '(1 -1 -1 1 0)
    (subseq (nreverse (pairlis (list - - u d u u d d -)  ; row
                               (list r l - - r l l r -))) ; column
            0 below)))" n)
(arrayloop
 "(loop for o below (* h w) for oi = (floor o w) and oj = (mod o w)
        do (loop for (di . dj) in (crosspair (range " p ") (range " p ")) 
                 for i = (+ oi di)
                 for j = (+ oj dj)
                 unless (or (minusp i) (minusp j) (<= h i) (<= w j))
                   do " p ")
        finally (prina " p "))")
(prina
 "(defun prina (2d-array)
  (destructuring-bind (x y) (array-dimensions 2d-array)
    (loop for i below x
          do (loop for j below y
                   do (format t \"~a\" (aref 2d-array i j)))
             (format t \"~%\"))))" n)
(copy-array
 "(defun copy-array (2d-array)
  (destructuring-bind (h w) (array-dimensions 2d-array)
    (make-array (list h w)
                :initial-contents
                (loop for i below h
                      collect (loop for j below w
                                    collect (aref 2d-array i j))))))" n)
(transpose
 "(defun transpose (lst)
  (apply #'map 'list #'list lst))" n)
(transpose2darray
 "(defun transpose (2d-array)
  (destructuring-bind (h w) (array-dimensions 2d-array)
    (loop for i below h with array = (make-array (list w h))
          do (loop for j below w
                   do (setf (aref array j i) (aref 2d-array i j)))
          finally (return array))))" n)
(sequences->2d-array
 "(defun sequences->2d-array (sequences &key (initial-element nil))
  (let* ((h (length sequences))
         (w (apply #'max (mapcar #'length sequences)))
         (array (make-array (list h w) :initial-element initial-element)))
    (loop for seq in sequences for i from 0
          do (loop for j from 0 below (length seq)
                   do (setf (aref array i j) (elt seq j)))
          finally (return array))))" n)
(2d-array->list
 "(defun 2d-array->list (2d-array)
  (destructuring-bind (h w) (array-dimensions 2d-array)
    (loop for i below h
          collect (loop for j below w
                        collect (aref 2d-array i j)))))" n)
(factorial
 "(defun factorial (n)
  (loop for i downfrom n to 1
        for v = i then (* v i)
        finally (return v)))" n)
(permutation
 "(defun permutation (n r)
  (loop for i downfrom n repeat r
        for v = i then (* v i)
        finally (return v)))" n)
(combination
 "(defun combination (n r)
  (loop for i downfrom n for j downfrom r repeat r
        for vn = i then (* vn i)
        for vr = j then (* vr j)
        finally (return (/ vn vr))))" n)
(permutations
 "(defun permutations (result-type sequence &optional (predicate #'<))
  (loop with ss = (sort sequence predicate)
        with is = (copy-seq ss) with rs = (reverse ss) until (equalp is rs)
        with ln = (length ss) with j = (1- ln) with i = (1- j)
        unless (funcall predicate (elt is i) (elt is (1+ i)))
          do (decf i)
        else
          unless (funcall predicate (elt is i) (elt is j))
            do (decf j)
          else
            do (rotatef (elt is i) (elt is j))
               (setf is (replace is
                                 (reverse (subseq is (1+ i)))
                                 :start1 (1+ i)))
               (setf j (1- ln))
               (setf i (1- j))
            and collect (copy-seq is) into it
        finally (return (coerce (cons ss it) result-type))))" n)
(next-permutation
 "(defun next-permutation (sequence &optional (predicate #'<))
  (loop with j = (1- (length sequence)) with i = (1- j)
        unless (funcall predicate (elt sequence i) (elt sequence (1+ i)))
          do (when (minusp (decf i)) (return nil))
        else
          unless (funcall predicate (elt sequence i) (elt sequence j))
            do (decf j)
          else
            do (rotatef (elt sequence i) (elt sequence j))
               (return (replace sequence
                                (reverse (subseq sequence (1+ i)))
                                :start1 (1+ i)))))" n)
(nextpermutationloop
 "(loop for str = (sort s #'char<) then (next-permutation str #'char<) while str
        )")
(palindromep
 "(defun palindromep (sequence &optional (predicate #'eq))
  (loop with sq = (coerce sequence 'vector)
        for i to (floor (length sq) 2)
        always (funcall predicate (elt sq i) (elt sq (- (length sq) 1 i)))))" n)
(logdigit
 "(defun logdigit (index integer)
  (let* ((str (write-to-string integer))
         (len (length str)))
    (and (< index len)
         (digit-char-p (elt str (- len index 1))))))" n)
(cycleloop
 "(loop with ceil
        repeat ceil
        for i = " p " then (mod (1+ i) ceil)
        )
        " n> r> ")")
(hourloop
 "(loop with str = s with end = u
        for h = str then (mod (1+ h) 24) until (= h end)
        )" n)
(timeloop
 "(loop with strh =  with strm = 
        with endh =  with endm =
        until (and (= h endh) (= m endm))
        repeat (* 24 60)
        for m = strm then (mod (1+ m) 60)
        for h = strh then (if (zerop m) (mod (1+ h) 24) h)
        )" n)
(bitsearch
 "(loop with digit = " p " for d below (ash 1 digit)
        do (loop for i below digit
                 when (logbitp i d)
                   do " n> r> "))")
(cross
 "(defun cross (complex1 complex2)
  (- (* (realpart complex1) (imagpart complex2))
     (* (imagpart complex1) (realpart complex2))))" n)
(dot
 "(defun dot (complex1 complex2)
  (+ (* (realpart complex1) (realpart complex2))
     (* (imagpart complex1) (imagpart complex2))))" n)
(ccwp
 "(defun ccwp (complex1 complex2)
  (plusp (cross complex1 complex2)))" n)
(noyes
 "~:[No~;Yes~]")
(binarysearch
 "(defun binary-search (l r test-fn)
  (loop for m = (floor (+ l r) 2)
        while (< l m r)
        do (if (funcall test-fn m)
               (setf l m)
               (setf r m))
        finally (return (if (funcall test-fn r) r m))))" n)
(recursion
 "(labels ((recursion (lst acc)
             (if " p "
              )))
    (recursion nil nil))")
(declf
 "(defmacro declf (place floor)
  `(setf ,place (max (1- ,place) ,floor)))" n)
(inclf
 "(defmacro inclf (place ceil)
  `(setf ,place (min (1+ ,place) ,ceil)))" n)
(dfsloop
 "(let ((adj (make-hash-table)))
    (loop for (a . b) in nds
          do (push a (gethash b adj))
             (push b (gethash a adj)))
    (loop for k being the hash-key in adj
          with vst = (make-hash-table) and stack
          unless (gethash k vst)
            do (setf (gethash k vst) t)
               (push k stack)
               (loop while stack for n = (pop stack)
                     do (loop for a in (gethash n adj)
                              unless (gethash a vst)
                                do (setf (gethash a vst) t)
                                   (push a stack)))
          finally (loop for k being the hash-key in vst
                        do (format t \"~a \" k))))")
(dfsrecursion
 "(let ((adj (make-hash-table))
        (vst (make-hash-table)))
    (loop for (a . b) in nds
          do (push a (gethash b adj))
             (push b (gethash a adj)))
    (labels ((dfs (x)
               (recursion (list x)))
             (recursion (acc)
               (cond ((= (car acc) y)
                      (reverse acc))
                     (t
                      (setf (gethash (car acc) vst) t)
                      (loop for a in (gethash (car acc) adj)
                            unless (gethash a vst)
                              do (setf (gethash a vst) t)
                                 (recursion (cons a acc)))))))
      (dfs x)))" n)
(unionbyrank
 "(let (parent rank)
  (defun uf-init (size)
    (setf rank (make-array size :initial-element 0))
    (setf parent (make-array size
                             :initial-contents
                             (loop for i below size collect i))))

  (defun uf-root (x &key (test #'=))
    (labels ((root (x acc)
               (let ((p (elt parent x)))
                 (cond ((funcall test p x)
                        (mapcar #'(lambda (a) (setf (elt parent a) x)) acc)
                        x)
                       (t
                        (root p (cons x acc)))))))
      (root x nil)))

  (defun uf-samep (x y &key (test #'=))
    (funcall test (uf-root x :test test) (uf-root y :test test)))

  (defun uf-unite (x y &key (test #'=))
    (let ((x (uf-root x :test test))
          (y (uf-root y :test test)))
      (unless (funcall test x y)
        (let ((rx (elt rank x))
              (ry (elt rank y)))
          (cond ((< rx ry)
                 (setf (elt parent x) y))
                (t
                 (if (= rx ry)
                     (incf (elt rank x)))
                 (setf (elt parent y) x)))))))

  (defun uf-ranks ()
    rank)

  (defun uf-roots ()
    (loop for i below (length parent) do (uf-root i) finally (return parent)))

  (defun uf-force (list)
    (setf parent (coerce list 'vector))))" n)
(unionfind
 "(defun make-union-find (size &key (test #'=))
  \"return data structure for union by size.\"
   (cons (make-array size :initial-element -1) test))

(defgeneric (setf parent-union) (new x union-table))

(defmethod parent-union (x union-table)
  (elt (car union-table) x))

(defmethod (setf parent-union) (new x union-table)
  (setf (elt (car union-table) x) new))

(defun root-union (x union-table)
  (if (minusp (parent-union x union-table))
      x
      (setf (parent-union x union-table)
            (root-union (parent-union x union-table) union-table))))

(defun size-union (x union-table)
  (- (parent-union (root-union x union-table) union-table)))

(defun samep-union (x y union-table)
  (destructuring-bind (parent-or-size . test) union-table
    (funcall test (root-union x union-table) (root-union y union-table))))

(defun unite-union (x y union-table)
  (destructuring-bind (parent-or-size . test) union-table
    (let ((x (root-union x union-table))
          (y (root-union y union-table)))
      (unless (funcall test x y)
        (if (< (parent-union y union-table) (parent-union x union-table))
            (rotatef x y))
        (incf (parent-union x union-table) (parent-union y union-table))
        (setf (parent-union y union-table) x)))))" n)
(queue
 "(defclass queue ()
  ((list :initform nil)
   (tail :initform nil)))

(defun make-queue ()
  (make-instance 'queue))

(defmethod print-object ((queue queue) stream)
  (print-unreadable-object (queue stream :type t)
    (with-slots (list tail) queue
      (cond ((nthcdr 4 list)
             (format stream "(~{~S ~}... ~S)"
                     (subseq list 0 3) (first tail)))
            (t
             (format stream "~:S" list))))))

(defmethod dequeue ((queue queue))
  (with-slots (list) queue
    (pop list)))

(defmethod enqueue (new-item (queue queue))
  (with-slots (list tail) queue
    (let ((new-tail (list new-item)))
      (if (null list)
          (setf list new-tail)
          (setf (cdr tail) new-tail))
      (setf tail new-tail)))
  queue)" n)
;; Local Variables:
;; mode: lisp-data
;; outline-regexp: "[a-z]"
;; End:
