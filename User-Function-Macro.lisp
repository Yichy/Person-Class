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

(defun string-in (char str)
  (let (rslt)
    (dotimes (i (length str) rslt)
      (if (string= char (char str i))
          (setf rslt t)))))

(defun type-eq (&rest args)
  (let ((type (type-of (pop args))))
    (mapcar #'(lambda (x) (if (equal type (type-of x)) t (return-from ))))))
