(defparameter classes-path (make-pathname :name "Classes.lisp"))
(defparameter person-path (make-pathname :name "Person.lisp"))
(defparameter person-path-m (make-pathname :name "%Person.lisp&=%"))
(defparameter save-path (make-pathname :name "Save-Persons.lisp"))
(defparameter load-path (make-pathname :name "Load-Person.lisp"))

(defmacro add-class (name-of-class parents variables)
  (let ((name-of-classg (gensym)) (parentsg (gensym)) (variablesg (gensym)))
    (with-open-file (str classes-path :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      `(let ((,name-of-classg ,name-of-class) (,parentsg ,parents) (,variablesg ,variables))
         (format ,str "(defclass ~A ~A(" ,name-of-classg ,parentsg)
         (dolist (i ,variablesg)
           (format ,str "~%    ~A" i))
      (format ,str "))")))))

(defun class-printer (str name-of-class name-of-macro variables)
  (format str "
    (defmacro ~A ()~%
       `(make-instance '~A~%
                      :name ,nameg~%
                      :gender ,genderg~%
                      :born-in ,born-ing~%
                      :live-in ,live-ing" name-of-macro name-of-class)
  (mapcar #'(lambda (x)
              (format str "~%                      :~A ,~A"
                      (car x) (cdr x)))
          variables)
  (format str "))~%"))

(defun class-let-printer (str variables)
  (mapcar #'(lambda (x)
              (format str "        (~A ~A)~%" (car x) (cdr x)))
          variables))

(defun class-keys-printer (str variables)
  (mapcar #'(lambda (x)
              (format str
                      "                                                     ~A~%"
                      (car x)))
          variables))

(defmacro add-maker (name-of-class name-of-macro parents variables)
  (let ((name-of-classg (gensym))
        (parentsg (gensym))
        (variablesg (gensym))
        (name-of-macrog (gensym)))
    (with-open-file (str1 person-path :direction :input
                         :if-exists :append
                         :if-not-exists :create)
      (with-open-file (str2 person-path-m :direction :output
                           :if-exists :append
                           :if-not-exists :create)
        `(let ((,name-of-classg ,name-of-class)
               (,parentsg ,parents)
               (,variablesg ,variables)
               (,name-of-macrog ,name-of-macro))
           (do ((s (read-line str1 nil 'eof) (read-line str1 nil 'eof)))
               ((equal s 'eof) (copy-file person-path-m person-path))
             (format ,str2 "~A~%" s)
             (if (string-in ";!" s)
                 (class-printer ,str2 ,name-of-classg ,name-of-macrog ,variablesg))
             (if (string-in ";let" s)
                 (class-let-printer ,str2 ,variablesg))
             (if (string-in ";keys" s)
                 (class-keys-printer ,str2 ,variablesg))))))))
