;;;; Schema definition, creation and management

(in-package #:steampunk.schema)
(cl-annot:enable-annot-syntax)

(defvar *db-sequences* '()
  "Stores defined database sequences.")

(defparameter *db-tables* '()
  "Stores list of database tables for proper initialization.")

(defparameter *db-entities* ()
  "Database entities.")

;;; **************************************************************************
;;;  General
;;; **************************************************************************

(define-condition name-not-supplied ()
  ((object :initarg :object))
  (:documentation "Raised when some name wasn't supplied at creation.")
  (:report (lambda (condition stream)
             (with-slots (object) condition
                 (format stream "~S name wasn't supplied"
                         object)))))

;;; **************************************************************************
;;;  Initialization
;;; **************************************************************************

(defgeneric perform-database-init (object)
  (:documentation "Does necessary actions to ensure object exists in database.
This currently involves tables and sequence creation."))

(defgeneric perform-database-deinit (object)
  (:documentation "Does necessary actions to ensure object is properly cleaned up.
This currently involves tables and sequence destruction."))

@export
(defun init-database ()
  "Initializes database for application needs.

It does the following:
* Creates defined tables
* Creates defined sequences"
  (mapc #'perform-database-init *db-sequences*)
  (mapc #'perform-database-init (reverse *db-tables*))
  (values))

@export
(defun deinit-database (&optional you-sure? you-absolutely-sure?)
  "WARNING: Destroys all the data in database that was created by application.
Requires user to supply T as first argument and :YES as second in order to proceed.

This is usually not needed and should only be used in debugging from REPL hence the
arguments."
  (cond
    ((not (and (eql t you-sure?) (eql :yes you-absolutely-sure?)))
     (format t "You have to supply T as first argument as :YES as second one to proceed."))
    (t
     (mapcar #'perform-database-deinit (reverse *db-tables*))
     (mapcar #'perform-database-deinit *db-sequences*)))
  (values))

;;; **************************************************************************
;;;  Sequences
;;; **************************************************************************

(define-condition sequence-name-not-supplied (name-not-supplied) ()
  (:documentation "Raised when sequence name wasn't supplied at creation."))

(defclass db-sequence ()
  ((name :initarg :name
         :type symbol)
   (start :initarg :start
          :initform 0
          :type fixnum)
   (min-value :initarg :min-value
              :initform 0
              :type fixnum)
   (max-value :initarg :max-value
              :initform (1- (expt 2 64))
              :type fixnum)
   (increment :initarg :increment
              :initform 1
              :type fixnum)
   (cycle? :initarg :cycle?
           :initform nil
           :type boolean)
   (cache :initarg :cache
          :initform 1
          :type fixnum))
  (:documentation "Stores information about defined database sequence."))

(defmethod initialize-instance :after ((instance db-sequence) &rest initargs)
  "Ensures that sequence has a name. Otherwise, signals SEQUENCE-NAME-NOT-SUPPLIED error."
  (declare (ignore initargs))
  (unless (slot-boundp instance 'name)
    (error 'sequence-name-not-supplied
           :sequence instance)))

(defun sequence-defined-p (name)
  "Checks if sequence has already been defined by finding sequence with the same NAME in defined
sequences list."
  (when (find name *db-sequences*
              :test #'string=
              :key #'(lambda (sequence)
                       (slot-value sequence 'name)))
    t))

(defmethod perform-database-init ((sequence db-sequence))
  "Creates database sequence. Ensures that it doesn't exist first. If it exists, does nothing."
  (unless (steampunk.database:sequence-exists-p sequence)
    (with-slots (name start min-value max-value increment cycle? cache)
        sequence
      (steampunk.database:execute (:create-sequence name
                                   :start start
                                   :min-value min-value
                                   :max-value max-value
                                   :increment increment
                                   :cycle cycle?
                                   :cache cache)))))

(defmethod perform-database-deinit ((sequence db-sequence))
  "Destroys database sequence if it exists. If it doesn't, does nothing."
  (with-slots (name)
      sequence
    (steampunk.database:execute (:drop-sequence :if-exists name))))

@export
(defmacro defsequence (name &key start min-value max-value increment cycle? cache)
  "Defines new database sequence with given NAME and various initargs for further creation by
INIT-DATABASE. If sequence with the same name already exists, it does nothing and returns NIL."
  `(unless (sequence-defined-p name)
     (push (make-instance 'db-sequence
                          :name ,name
                          ,@(when start
                              `(:start ,start))
                          ,@(when min-value
                              `(:min-value ,min-value))
                          ,@(when max-value
                              `(:max-value ,max-value))
                          ,@(when increment
                              `(:increment ,increment))
                          ,@(when cycle?
                              `(:cycle? ,cycle?))
                          ,@(when cache
                              `(:cache ,cache)))
           *db-sequences*)))

;;; **************************************************************************
;;;  Tables
;;; **************************************************************************

(define-condition table-name-not-supplied (name-not-supplied) ()
  (:documentation "Raised when table name wasn't supplied at creation."))

(defclass db-table-foreign-key ()
  ((foreign-table-name :initarg :foreign-table-name
                       :type symbol)
   (foreign-column-name :initarg :foreign-column-name
                        :type symbol)
   (local-column-name :initarg :local-column-name
                      :type symbol)
   (on-delete :initarg :on-delete
              :initform :cascade
              :type keyword)
   (on-update :initarg :on-update
              :initform :cascade
              :type keyword))
  (:documentation "Stores information about foreign table mappings."))

(defclass db-table ()
  ((name :initarg :name
         :type symbol)
   (db-object-mapping :initarg :db-object-mapping
                      :initform nil
                      :type symbol)
   (foreign-keys :initarg :foreign-keys
                 :initform '()
                 :type list)
   (unique :initarg :unique
           :initform '()
           :type list)
   (unique-index-on :initarg :unique-index-on
                    :initform '()
                    :type list)
   (index-on :initarg :index-on
             :initform '()
             :type list))
  (:documentation "Stores information about database table."))

(defmethod initialize-instance :after ((instance db-table) &rest initargs)
  "Ensures that table has a name. Otherwise, signals TABLE-NAME-NOT-SUPPLIED error."
  (declare (ignore initargs))
  (unless (slot-boundp instance 'name)
    (error 'table-name-not-supplied
           :table instance)))

(defun table-defined-p (name)
  "Checks if table has already been defined by finding table with the same NAME in defined
tables list."
  (when (find name *db-tables*
              :test #'string=
              :key #'(lambda (table)
                       (slot-value table 'name)))
    t))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (define-condition invalid-table-definition-form ()
    ((form :initarg :form)
     (message :initarg :message))
    (:documentation "Raised when table definition form is improper in some way.")
    (:report (lambda (condition stream)
               (with-slots (form message) condition
                 (format stream "~S isn't valid table definition form"
                         form)
                 (format stream "Reason: ~A"
                         message)))))

  (defun symbol-arrow-p (symbol)
    "Checks if SYMBOL is ->."
    (string= "->" symbol))

  (defun match-foreign-key-arguments (arguments)
    "Does pattern matching of foreign key ARGUMENTS."
    (optima:match arguments
      ((or (list local-column (satisfies symbol-arrow-p) (list foreign-table foreign-column))
           (list local-column (satisfies symbol-arrow-p) (list foreign-table foreign-column)
                 ':on-delete on-delete)
           (list local-column (satisfies symbol-arrow-p) (list foreign-table foreign-column)
                 ':on-update on-update)
           (list local-column (satisfies symbol-arrow-p) (list foreign-table foreign-column)
                 ':on-delete on-delete
                 ':on-update on-update)
           (list local-column (satisfies symbol-arrow-p) (list foreign-table foreign-column)
                 ':on-update on-update
                 ':on-delete on-delete))
       (list :local-column-name local-column
             :foreign-table-name foreign-table
             :foreign-column-name foreign-column
             :on-delete on-delete
             :on-update on-update))))

  (defun make-foreign-key (arguments)
    "Makes instance of foreign key with given ARGUMENTS."
    (let* ((parsed (match-foreign-key-arguments arguments))
           (foreign-table-name (getf parsed :foreign-table-name))
           (foreign-column-name (getf parsed :foreign-column-name))
           (local-column-name (getf parsed :local-column-name))
           (on-delete (getf parsed :on-delete))
           (on-update (getf parsed :on-update)))
      (make-instance 'db-table-foreign-key
                     :foreign-table-name foreign-table-name
                     :foreign-column-name foreign-column-name
                     :local-column-name local-column-name
                     ;; TODO(me@hitecnologys.org): Do something about defaults.
                     :on-delete (or on-delete :cascade)
                     :on-update (or on-update :cascade))))

  (defun read-table-form (form)
    "Parses single table FORM and returns two values: form type and data.

The following form types exists:
* :DB-OBJECT-MAPPING
* :FOREIGN-KEY
* :UNIQUE-INDEX-ON
* :INDEX-ON"
    (destructuring-bind (operator &rest arguments) form
      (switch (operator :test #'string=)
        ('db-object-mapping
         (values :db-object-mapping
                 (first arguments)))
        ('foreign-key
         (values :foreign-key
                 (make-foreign-key arguments)))
        ('unique
         (values :unique
                 (first arguments)))
        ('unique-index
         (values :unique-index-on
                 (first arguments)))
        ('index
         (values :index-on
                 (first arguments)))
        (otherwise
         (error 'invalid-table-definition-form
                :form form
                :message (format nil "Unknown operator ~S" operator))))))

  (defun read-table-definition (name definition)
    "Iterates over table definition parsing it and collecting data for further storage in DB-TABLE
instance."
    (let (db-object-mapping
          foreign-keys
          unique
          unique-index-on
          index-on)
      (loop for form in definition
            do (multiple-value-bind (type data)
                   (read-table-form form)
                 (case type
                   (:db-object-mapping
                    (if (null db-object-mapping)
                        (setf db-object-mapping data)
                        (error 'invalid-table-definition-form
                               :form form
                               :message "Duplicate")))
                   (:foreign-key
                    (push data foreign-keys))
                   (:unique
                    (push data unique))
                   (:unique-index-on
                    (push data unique-index-on))
                   (:index-on
                    (push data index-on)))))
      (make-instance 'db-table
                     :name name
                     :db-object-mapping db-object-mapping
                     :foreign-keys foreign-keys
                     :unique unique
                     :unique-index-on unique-index-on
                     :index-on index-on)))

  (defun make-table-compiled-form (operator args-list)
    "Iterates over an ARGS-LIST and makes lists of format `(OPERATOR ,@ARG)"
    (loop for arg in args-list
          collecting `(,operator ,arg)))

  (defun make-db-object-compiled-form (db-object-name)
    "Makes DB-OBJECT reference form out of DB-OBJECT-NAME."
    `(let ((pomo:*table-symbol* ',db-object-name))
       (pomo:!dao-def)))

  (defun compile-foreign-key (foreign-key)
    "Compiles single FOREIGN-KEY instance."
    (with-slots (foreign-table-name
                 foreign-column-name
                 local-column-name
                 on-delete
                 on-update)
        foreign-key
      `(pomo:!foreign ',foreign-table-name ',local-column-name ',foreign-column-name
                      :on-delete ,on-delete
                      :on-update ,on-update)))

  (defun make-foreign-keys-compiled-form (foreign-keys)
    "Compiles FOREIGN-KEYS list."
    (loop for foreign-key in foreign-keys
          collecting (compile-foreign-key foreign-key)))

  (defun compile-table-definition (table)
    "Compiles DB-TABLE instance into a list acceptable by postmodern's DEFTABLE."
    (check-type table db-table)
    (with-slots (db-object-mapping foreign-keys unique-index-on index-on)
        table
      `(,(make-db-object-compiled-form db-object-mapping)
        ,@(make-table-compiled-form 'pomo:!index index-on)
        ,@(make-table-compiled-form 'pomo:!unique-index unique-index-on)
        ,@(make-foreign-keys-compiled-form foreign-keys)))))

(defmethod perform-database-init ((table db-table))
  "Creates database table. Ensures that it doesn't exist first. If it exists, does nothing."
  (unless (steampunk.database:table-exists-p table)
      (pomo:create-table table)))

(defmethod perform-database-deinit ((table db-table))
  "Destroys database table if it exists. If it doesn't, does nothing."
  (postmodern:execute (:drop-table :if-exists table)))

@export
(defmacro deftable (table-name &body definition)
  "Defines new table named TABLE-NAME.
If table with the same name has already been defined, does nothing and returns NIL.

Definition is a list of forms of the following format:
(:key value-1 value-2 ... value-n)

Key can be one of the following:
* :DB-OBJECT-MAPPING
* :FOREIGN-KEY
* :UNIQUE
* :UNIQUE-INDEX-ON
* :INDEX-ON

:DB-OBJECT-MAPPING defines DAO which slots are to be table columns. It has the following format:
(:db-object-mapping dao-name)

:FOREIGN-KEY defines mappings to foreign tables. It has the following format:
(:foreign-key local-column -> (foreign-table foreign-column))

:UNIQUE defines unique columns. It has the following format:
(:unique column-name)

:UNIQUE-INDEX-ON defines unique indexes. It has the following format:
(:unique-index-on column-name)

:INDEX-ON defines indexes. It has the following format:
(:index-on column-name)"
  (let* ((table-object (read-table-definition table-name definition))
         (compiled-table-definition (compile-table-definition table-object)))
    (unless (table-defined-p table-name)
       (push table-object *db-tables*))
    `(pomo:deftable ,table-name
       ,@compiled-table-definition)))

;;; **************************************************************************
;;;  ORM
;;; **************************************************************************

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass db-class (pomo:dao-class) ()
    (:documentation "Metaclass for ORM.")))

(defclass db-object () ()
  (:metaclass db-class)
  (:documentation "Base class for object-relational mapping."))

@export
(defmacro defdao (name direct-superclasses direct-slots &rest options)
  "Defines new DAO.

This is one-to-one mapping to postmodern's DAO except that it sets metaclass to DB-CLASS for you and
inexplicitly inherits DB-OBJECT."
  `(defclass ,name ,(list* 'db-object direct-superclasses)
     ,direct-slots
     ,@options
     (:metaclass db-class)))

;;; **************************************************************************
;;;  ORM+
;;; **************************************************************************

(define-condition entity-name-not-supplied (name-not-supplied) ()
  (:documentation "Raised when entity name wasn't supplied at creation."))

(defclass db-entity ()
  ((name :initarg :name
         :type symbol
         :reader name)
   (fields :initarg :fields
           :type list)
   (db-object :initarg :db-object
              :initform nil
              :type symbol)
   (db-table :initarg :db-table
             :initform nil
             :type symbol)
   (relations :initarg :relations
              :initform '()
              :type list)
   (table-name :initarg :table-name
               :type symbol))
  (:documentation "Stores information about database entity."))

(defclass db-entity-field ()
  ((name :initarg :name
         :type symbol
         :reader name)
   (initarg :initarg :initarg)
   (initform :initarg :initform)
   (db-type :initarg :db-type
            :type symbol)
   (db-initform :initarg :db-initform
                :type list)
   (docstring :initarg :docstring
              :type (or string null))
   (type :initarg :type
         :type symbol)
   (primary? :initarg :primary?
             :initform nil
             :type boolean
             :reader primary-p)
   (index? :initarg :index?
           :initform nil
           :type boolean
           :reader index-p)
   (unique? :initarg :unique?
            :initform nil
            :type boolean
            :reader unique-p)
   (unique-index? :initarg :unique-index?
                  :initform nil
                  :type boolean
                  :reader unique-index-p))
  (:documentation "Stores information about entity field."))

(defclass db-entity-relation ()
  ((entity :initarg :entity
           :type db-entity)
   (source-field :initarg :source-field
                 :type symbol)
   (target-field :initarg :target-field
                 :type symbol))
  (:documentation "Stores information about entity relation to another entity."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-condition unknown-entity-option ()
    ((entity :initarg :entiry)
     (option :initarg :option))
    (:documentation "Raised when an unknown entity option is met.")
    (:report (lambda (condition stream)
               (with-slots (entity option) condition
                 (format stream "Option ~S is incorrect option for entity ~S."
                         option entity)))))

  (defun match-entity-relation-arguments (arguments)
    "Matches relation arguments against a pattern, then returns useful data."
    (optima:match arguments
      ((list source-field (satisfies symbol-arrow-p) (list entity target-field))
       (list source-field entity target-field))))

  (defun get-entity (name)
    "Retrieves entity by its NAME."
    (find name *db-entities*
          :key #'name))

  (defun parse-entity-relation (relation)
    "Parses entity relation into DB-ENTITY-RELATION object.
Relations have the same format as foreign keys in DEFTABLE because they essentials are the same
thing."
    (destructuring-bind (source-field entity target-field)
        (match-entity-relation-arguments relation)
      (make-instance 'db-entity-relation
                     :entity (get-entity entity)
                     :source-field source-field
                     :target-field target-field)))

  (defun parse-entity-relations (relations)
    "Iterates RELATIONS via PARSE-ENTITY-RELATION returning a list of it's calls."
    (mapcar #'parse-entity-relation relations))

  (defun get-entity-option-value (options option)
    "Retrieves OPTION value from list of OPTIONS.
OPTIONS is expected to be associative list with keywords in CAR and values in CDR."
    (cdr (assoc option options)))

  (defun parse-entity-field (field)
    "Parses entity fields into DB-ENTITY-FIELD object."
    (destructuring-bind (name &rest keys)
        field
      (let ((parsed-field (make-instance 'db-entity-field
                                         :name name
                                         :primary? (getf keys :primary)
                                         :index? (getf keys :index)
                                         :unique? (getf keys :unique)
                                         :unique-index? (getf keys :unique-index))))
        (with-slots (db-type db-initform initarg initform docstring type)
            parsed-field
          (loop for (key slot) in '((:db-type db-type)
                                    (:db-initform db-initform)
                                    (:initform initform)
                                    (:initarg initarg)
                                    (:documentation docstring)
                                    (:type type))
                doing (awhen (getf keys key)
                        (setf (slot-value parsed-field slot) it))))
        parsed-field)))

  (defun parse-entity-fields (fields)
    "Iterates over FIELDS via PARSE-ENTITY-FIELD."
    (mapcar #'parse-entity-field fields))

  (defun compile-entity-field (field)
    "Compiles single entity FIELD to slot form for DEFEAT."
    (with-slots (name initform initarg db-type db-initform docstring type)
        field
      `(,name ,@(when (slot-boundp field 'initform)
                  `(:initform ,initform))
              ,@(when (slot-boundp field 'initarg)
                  `(:initarg ,initarg))
              ,@(when (slot-boundp field 'docstring)
                  `(:documentation ,docstring))
              ,@(when (slot-boundp field 'type)
                  `(:type ,type))
              ,@(when (slot-boundp field 'db-type)
                  `(:col-type ,db-type))
              ,@(when (slot-boundp field 'db-initform)
                  `(:col-default ,db-initform)))))

  (defun compile-entity-fields (fields)
    "Compiles entity FIELDS down to usable forms."
    (mapcar #'compile-entity-field fields))

  (defun find-primary-keys (parsed-fields)
    "Finds all entity's primary keys."
    (filter #'primary-p parsed-fields
            :key #'name))

  (defun compile-entity-db-object-options (name parsed-fields options)
    "Compiles DB-OBJECT options list."
    `((:table-name ,(or (first (get-entity-option-value options :table-name))
                        name))
      (:keys ,@(find-primary-keys parsed-fields))))

  (defun entity-table-name (entity)
    (slot-value entity 'table-name))

  (defun compile-entity-db-table-options (parsed-fields parsed-relations)
    "Compiles options for DEFTABLE."
    (let (options)
      (flet ((push-option (option value)
               (pushnew `(,option ',value) options
                        :key #'first))
             (push-relation (option relation)
               (push `(,option ,@relation) options)))
        (dolist (field parsed-fields)
          (when (unique-p field)
            (push-option :unique (name field)))
          (when (index-p field)
            (push-option :index (name field)))
          (when (unique-index-p field)
            (push-option :unique-index (name field))))
        (dolist (relation parsed-relations)
          (with-slots (entity source-field target-field)
              relation
            (push-relation :foreign-key `(,source-field -> (,(entity-table-name entity) ,target-field))))))
      options))

  (defun make-entity (name &key fields relations table-name)
    "Makes DB-ENTITY instance."
    (make-instance 'db-entity
                   :name name
                   :fields fields
                   :relations relations
                   :table-name table-name
                   :db-object name
                   :db-table table-name)))

(defun entity-defined-p (name)
  "Checks if entity has already been defined by finding entity with the same NAME in defined
entities list."
  (when (find name *db-entities*
              :test #'string=
              :key #'(lambda (entity)
                       (slot-value entity 'name)))
    t))

@export
(defmacro defentity (name (&rest parents) (&rest fields) (&rest relations) &rest options)
  "Defines new database entity. Entity is a class that is used to access database
  records. Basically, it's DAO with additional features.

PARENTS is a list of classes or other entities that this entity inherits.

FIELDS is a list of slots. Each slot can have the following args:
  :db-type       | type of mapped database column
  :db-initform   | initialization form of mapped database column
  :initform      | initialization form of DAO instance
  :initarg       | initarg for MAKE-INSTANCE
  :documentation | docstring
  :type          | type of slot value

RELATIONS is a list of foreign tables mapped to this entity. Each list element has the following
format:
  (local-slot -> (foreign-entity foreign-slot))

OPTIONS is a associative list of options to entity itself. Currently the following options are
available:
  :table-name    | name of database table associated with entity"
  (let* ((parsed-fields (parse-entity-fields fields))
         (parsed-relations (parse-entity-relations relations))
         (compiled-fields (compile-entity-fields parsed-fields))
         (db-object-options (compile-entity-db-object-options name parsed-fields options))
         (db-table-options (compile-entity-db-table-options parsed-fields parsed-relations))
         (table-name (or (first (get-entity-option-value options :table-name))
                         name)))
    (unless (entity-defined-p name)
      (push (make-entity name
                         :fields parsed-fields
                         :relations parsed-relations
                         :table-name table-name)
            *db-entities*))
    `(progn
       (defdao ,name (,@parents)
         (,@compiled-fields)
         ,@db-object-options)
       (deftable ,table-name
         (:db-object-mapping ,name)
         ,@db-table-options))))

;;; **************************************************************************
;;;  Relations
;;; **************************************************************************

@export
(defmacro defrelation (name &body relations)
  "Defines new relation between two entities.

RELATIONS is a list of foreign mappings. Each list element has the following format:
  (local-slot -> (foreign-entity foreign-slot))"
  (let* ((parsed-relations (parse-entity-relations relations))
         (db-table-options (compile-entity-db-table-options () parsed-relations)))
    `(deftable ,name
       ,@db-table-options)))
