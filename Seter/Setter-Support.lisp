(defun class-macro-printer (str name-of-class name-of-macro variables)
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

(defun class-test-rslt-printer (str num name-of-class name-of-macro)
  (format str"
         (~A (format t \"  -~A~A\")~%
          (~A))~%"num name-of-class '~% name-of-macro))
