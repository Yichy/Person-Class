(defun add-person (&rest args)
  (mapcar #'(lambda (arg)
              (vector-push-extend arg *persons*))
          args))

(defun get-person-type (string)
  (cond
    ((string-in ";person" string) 'person)
    ((string-in ";legal-person" string) 'legal-person)
    ((string-in ";real-person" string) 'real-person)
    ((string-in ";real-legal-person" string) 'real-legal-person)))
