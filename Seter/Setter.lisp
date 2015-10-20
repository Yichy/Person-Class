(load "Seter/Setter-Support.lisp")

(defparameter classes-path (make-pathname :name "Core/Classes.lisp"))
(defparameter person-path (make-pathname :name "Core/Person.lisp"))
(defparameter person-path-m (make-pathname :name
                                           "temp/%Person.lisp&=%"))
(defparameter save-path (make-pathname :name "Save-Persons.lisp"))
(defparameter load-path (make-pathname :name "Load-Person.lisp"))

(defmacro add-class (name-of-class parents variables)
  (let ((name-of-classg (gensym)) (parentsg (gensym)) (variablesg (gensym)))
    (with-open-file (str classes-path :direction :output
                         :if-exists :append
                         :if-does-not-exist :create)
      `(let ((,name-of-classg ,name-of-class) (,parentsg ,parents)
             (,variablesg ,variables))
         (format ,str "(defclass ~A ~A(" ,name-of-classg ,parentsg)
         (dolist (i ,variablesg)
           (format ,str "~%    ~A" i))
      (format ,str "))")))))

(defmacro add-maker (name-of-class name-of-macro parents variables)
  (let ((name-of-classg (gensym))
        (parentsg (gensym))
        (variablesg (gensym))
        (name-of-macrog (gensym))
        (numg (gensym)))
    (with-open-file (str1 person-path :direction :input
                         :if-exists :append
                         :if-does-not-exist :create)
      (with-open-file (str2 person-path-m :direction :output
                           :if-exists :supersede
                           :if-does-not-exist :create)
        `(let ((,name-of-classg ,name-of-class)
               (,parentsg ,parents)
               (,variablesg ,variables)
               (,name-of-macrog ,name-of-macro)
               (,numg 0))
           (do ((s (read-line str1 nil 'eof) (read-line str1 nil 'eof)))
               ((equal s 'eof) (copy-file person-path-m person-path))
             (format ,str2 "~A~%" s)
             (if (string-in ";number" s) (incf ,numg))
             (case (get-make-type s)
               (:macro-printer
                (class-macro-printer ,str2 ,name-of-classg ,name-of-macrog
                                     ,variablesg))
               (:let-printer
                (class-let-printer ,str2 ,variablesg))
               (:keys-printer
                (class-keys-printer ,str2 ,variablesg))
               (:test-rslt-printer
                (class-test-rslt-printer ,str2 ,numg ,name-of-classg
                                         ,name-of-macrog)))))))))
