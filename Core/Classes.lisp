(defclass person ()
  ((name :accessor name
	 :initarg :name
	 :initform nil)
   (gender :accessor gender
	   :initarg :gender
	   :initform (error "Person must have gender."))
   (born-in :accessor born-in
	    :initarg :born-in
	    :initform (error "Person must born in some place."))
   (live-in :accessor live-in
	    :initarg :live-in
	    :initform (error "Person must live in some place."))
   (id :accessor id
       :initform (incf *person-id*))))

(defclass legal-person (person)
  ((nationality :accessor nationality
		:initarg :nationality
		:initform (error "legal-person must have nationality."))))

(defclass real-person (person)
  ((person-character :accessor person-character
                     :initarg :person-character
                     :initform "real-person must have character.")
   (parents :accessor parents
            :initarg :parents
            :initform "real-person must have parents.")))

(defclass real-legal-person (real-person legal-person) ())

