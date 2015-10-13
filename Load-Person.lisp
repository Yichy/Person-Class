(defmacro load-persons ()
  (let ((thing (gensym)) (buf (gensym)))
    `(with-open-file (str ,path :direction :input
                          :if-exists :append
                          :if-does-not-exist :create)
       (do ((,buf nil)
            (,thing (read str nil 'eof)
                    (read str nil 'eof)))
           ((equal ,thing 'eof) t)
         (if (equal ,thing '!)
             (progn
               (case (length ,buf)
                 (4 (add-person (make-person (car ,buf) (nth 1 ,buf) (nth 2 ,buf)
                                             (nth 3 ,buf))))
                 (5 (add-person (make-person (car ,buf) (nth 1 ,buf) (nth 2 ,buf)
                                             (nth 3 ,buf) :nationality (nth 4 ,buf))))
                 (6 (add-person (make-person (car ,buf) (nth 1 ,buf) (nth 2 ,buf)
                                             (nth 3 ,buf) :character (nth 4 ,buf)
                                             :parents (nth 5 ,buf))))
                 (7 (add-person (make-person (car ,buf) (nth 1 ,buf) (nth 2 ,buf)
                                             (nth 3 ,buf) :character (nth 4 ,buf) :nationality
                                             (nth 5 ,buf) :parents (nth 6 ,buf)))))
               (setf ,buf nil))
             (back-push ,thing ,buf))))))
