(defgeneric test-person-type (
                              name
                              gender
                              born-in
                              live-in
                              character
                              nationality)
  (:documentation "Get person type from given args."))

(defmethod test-person-type (
                             name
                             gender
                             born-in
                             live-in
                             character
                             nationality)
  nil)

(defmethod test-person-type (
                             (name cons)
                             (gender symbol)
                             (born-in cons)
                             (live-in cons)
                             (character (eql nil))
                             (nationality (eql nil)))
  1);number

(defmethod test-person-type (
                             (name cons)
                             (gender symbol)
                             (born-in cons)
                             (live-in cons)
                             (character simple-vector)
                             (nationality (eql nil)))
  2);number

(defmethod test-person-type (
                             (name cons)
                             (gender symbol)
                             (born-in cons)
                             (live-in cons)
                             (character (eql nil))
                             (nationality cons))
  3);number

(defmethod test-person-type (
                             (name cons)
                             (gender symbol)
                             (born-in cons)
                             (live-in cons)
                             (character simple-vector)
                             (nationality cons))
  4);number

(defmacro make-person (name gender born-in live-in &key;keys
                                                     character
                                                     nationality)
  "name: a cons, person's name, include name and surname
gender: a symbol, person's gender, male or female
born-in: a cons, where person has born, include city name and country name
live-in: a cons, where person is living, include address to the dore
character: a length four array, person's character, use MBTI
nationality: a cons - for multy nationality, person's nationality,
                include country name"
  (let (;let
        (nameg (gensym))
        (genderg (gensym))
        (born-ing (gensym))
        (live-ing (gensym))
        (characterg (gensym))
        (nationalityg (gensym)));!
    (defmacro base-p ()
      `(make-instance 'person
                      :name ,nameg
                      :gender ,genderg
                      :born-in ,born-ing
                      :live-in ,live-ing))
    (defmacro legal-p ()
      `(make-instance 'legal-person
                      :name ,nameg
                      :gender ,genderg
                      :born-in ,born-ing
                      :live-in ,live-ing
                      :nationality ,nationalityg))
    (defmacro real-p ()
      `(make-instance 'real-person
                      :name ,nameg
                      :gender ,genderg
                      :born-in ,born-ing
                      :live-in ,live-ing
                      :person-character ,characterg))
    (defmacro rl-p ()
      `(make-instance 'real-legal-person
                      :name ,nameg
                      :gender ,genderg
                      :born-in ,born-ing
                      :live-in ,live-ing
                      :nationality ,nationalityg
                      :person-character ,characterg))
    `(let* ((,nameg ,name) (,genderg ,gender) (,born-ing ,born-in) (,live-ing ,live-in)
            (,characterg ,character) (,nationalityg ,nationality)
            (test-rslt
              (test-person-type
               ,nameg
               ,genderg
               ,born-ing
               ,live-ing
               ,characterg
               ,nationalityg)))
       (case test-rslt;case test-rslt
	 (1 (format t "  -Person~%")
	    (base-p))
	 (2 (format t "  -Real Person~%")
	    (real-p))
	 (3 (format t "  -Legal Person~%")
	    (legal-p))
	 (4 (format t "  -Real Legal Person~%")
	    (rl-p))))))
