(defmacro ntimes (n &rest body)
  (let ((i (gensym)) (times (gensym)))
    `(let ((,times ,n))
       (do ((,i 0 (+ ,i 1)))
	   ((>= ,i ,times))
	 ,@body))))

(defmacro cah (lst) `(car ,lst))

(define-modify-macro our-incf (&optional (y 1)) +)

(defmacro back-push (obj place)
  "obj: a symbol
place: a list

push obj to the back of the list"
  (let ((rplace (gensym)))
    `(let ((,rplace (reverse ,place)))
       (push ,obj ,rplace)
       (setf ,place (reverse ,rplace)))))

(defmacro for (var start end &rest body)
  "var: the varibal will contain number
start: a number, the starting number
end: a number, end at this number

create a for loop"
  (let ((endg (gensym)))
    `(do ((,var ,start (+ 1 ,var))
	  (,endg ,end))
	 ((> ,var ,endg))
       ,@body)))

(defmacro list-in (obj &rest args)
  (let ((gobj (gensym)) (rslt (gensym)))
    `(let ((,gobj ,obj) (,rslt nil))
       (or ,@(mapcar #'(lambda (x) `(if (eql ,gobj ,x) (setf ,rslt t)))
	       args))
       ,rslt)))

(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
                     choices)))))

(defmacro random-choice (&rest options)
  (let ((lst (gensym)) (len (gensym)))
    `(let ((,lst nil))
       ,@(mapcar #'(lambda (x) `(back-push ,x ,lst))
		 options)
       (let ((,len (length ,lst)))
	 (nth (random ,len) ,lst)))))

(defmacro random-pick (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
	    (mapcar #'(lambda (expr)
			`(,(incf key) ,expr))
		    exprs))))

(defmacro avg (&rest args)
  (let ((len (gensym)) (sum (gensym)))
    `(let ((len (length ,args)) (sum (+ ,@args)))
       (/ ,sum ,len))))

(defmacro with-gensym (args &rest body)
  `(let ,(mapcar #'(lambda (arg)
		     `(,arg (gensym)))
		 args)
     ,@body))

(defmacro aif (calculation then &optional else)
  `(let ((it ,calculation))
     (if it ,then ,else)))

(defun ask-num (&optional (ask-str "Please enter a number: "))
  "ask-str: the string it will print out when asking for a number

ask for a number, if user doesn't inupt a number, it will ask again"
  (format t "~A" ask-str)
  (let ((rslt (read)))
    (if (numberp rslt)
	rslt
	(progn
	  (format t "\"~A\" is ~A not NUMBER~%" rslt (type-of rslt))
	  (ask-num ask-str)))))

(defmacro filter (lst &optional (fn (lambda (e) e)))
  "lst: a list
fn: a test function

Return a new list that contains elements that
        returns true when apply to the test function."
  (let ((rslt (gensym)) (gfn (gensym)) (x (gensym)))
    `(let ((,rslt nil) (,gfn ,fn))
       (dolist (,x ,lst ,rslt)
	 (if (funcall ,gfn ,x)
	     (back-push ,x ,rslt))))))

(defun power (base index)
  (let ((rslt 1))
    (dotimes (i index rslt)
      (setf rslt (* rslt base)))))

(defmacro let-gensym (binding &body body)
  `(let ,(mapcar #'(lambda (bind) `(,(car bind) (gensym)))
                 binding)
     (let ,(mapcar #'(lambda (bind) `(,(car bind) ,@(cdr bind)))
		   binding)
       ,@body)))

(defun fundoc (function &optional (direction t))
  "function: a function or macro name
direction: the direction format function will output to

output doc-string of the function to the direction"
  (format direction "~A" (documentation function 'function)))

(defun char-in (char string)
  (let (rslt)
    (dotimes (i (length string) rslt)
      (if (string= char (char string i))
          (setf rslt t)))))

(defun string-in (target string)
  (if (< (length string) (length target))
      (return-from string-in nil))
  (dotimes (i (- (length string) (- (length target) 1)) nil)
    (if (string= target string :start2 i :end2 (+ i (length target)))
        (return-from string-in t))))

(defun type-eq (&rest args)
  (let ((type (type-of (pop args))))
    (mapcar #'(lambda (x) (if (equal type (type-of x)) t (return-from type-eq nil)))
            args))
    t)

(defun type-equal (&rest args)
  (let ((type (type-of (pop args))))
    (mapcar #'(lambda (x)
                (if (equal (if (equal (type-of type) cons) (car type) type)
                           (if (equal (type-of (type-of x)) cons) (car (type-of x)) (type-of x)))
                    t (return-from type-equal nil)))
            args))
  t)

(defun combine-list (lst1 lst2)
  `(,@lst1 ,@lst2))

(defun string-split (string &optional (split-when-t
                                       (lambda (x) (string= x " "))))
  (do ((i 1 (+ i 1)) rslt)
      ((or rslt (>= i (length string))) (if (not rslt) (list string) rslt))
    (if (funcall split-when-t (char string i))
        (setf rslt
              (combine-list
               (list (subseq string 0 i))
               (string-split (subseq string (+ i 1)) split-when-t))))))

(defun string-split-to-type (string)
  (multiple-value-bind (val next-pos) (read-from-string string)
    (combine-list `(,val)
                  (if (< next-pos (length string))
                      (string-split-to-type (subseq string next-pos))))))

(defun string-remove-after (string target)
  (do ((i 0 (+ i 1)))
      ((or (<= (length string) i)
           (string= target (char string i))) (subseq string 0 i))))
