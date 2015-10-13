(defparameter save-path (make-pathname :name "Persons.save"))

(defun set-save-path (file-name)
  (setf save-path (make-pathname :name file-name)))

(defgeneric save-person (obj)
  (:documentation "Save obj by given path."))

(defmethod save-person ((obj person))
  (with-open-file (str save-path :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "~A ~A ~A ~A !~%" (name obj) (gender obj) (born-in obj)
            (live-in obj))))

(defmethod save-person ((obj legal-person))
  (with-open-file (str save-path :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "~A ~A ~A ~A ~A !~%" (name obj) (gender obj) (born-in obj)
            (live-in obj) (nationality obj))))


(defmethod save-person ((obj real-person))
  (with-open-file (str save-path :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "~A ~A ~A ~A ~A ~A !~%" (name obj) (gender obj) (born-in obj)
            (live-in obj) (person-character obj) (parents obj))))

(defmethod save-person ((obj real-legal-person))
  (with-open-file (str save-path :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (format str "~A ~A ~A ~A ~A ~A ~A !~%" (name obj) (gender obj) (born-in obj)
            (live-in obj) (person-character obj) (nationality obj) (parents obj))))

(defun save-persons ()
  (dotimes (i (length *persons*) t)
    (save-person (aref *persons* i))))
