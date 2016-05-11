;;;; Database objects operations

(in-package #:steampunk.operations)
(cl-annot:enable-annot-syntax)

;;; **************************************************************************
;;;  CRUD operations
;;; **************************************************************************

;; ----------------
;;  Generic functions

@export
(defgeneric dao-exists-p (dao-class key value)
  (:documentation "Checks if DAO of class DAO-CLASS with KEY = VALUE exists."))

@export
(defgeneric create-dao (dao-class &rest initargs)
  (:documentation "Creates DAO of class DAO-CLASS with given INITARGS."))

@export
(defgeneric select-dao (dao-class predicate &rest ordering)
  (:documentation "Selects DAO of class DAO-CLASS from database with given predicate.
Also sorts the data according to ORDERING clauses."))

@export
(defgeneric query-dao (dao-class query)
  (:documentation "Queries DAO of class DAO-CLASS from database with given query."))

@export
(defgeneric update-dao (dao updates)
  (:documentation "Updates given DAO object fields.
UPDATES is a list of CONSes: (slot . new-value).
It also commits updated DAO to database in the :AFTER method."))

@export
(defgeneric delete-dao (dao)
  (:documentation "Deletes given DAO from database."))

@export
(defgeneric link-dao (dao-1 slot-1 dao-2 slot-2)
  (:documentation "Sets SLOT-1 of DAO-1 to value of SLOT-2 of DAO-2.
It also commits updated DAO to database in the :AFTER method."))

;; ----------------
;;  Methods

;; Symbol wrappers. Translate DAO-CLASS symbol to metaclass instance.

(defmethod dao-exists-p ((dao-class symbol) key value)
  (dao-exists-p (find-class dao-class) key value))

(defmethod create-dao ((dao-class symbol) &rest initargs)
  (apply #'dao-exists-p (find-class dao-class) initargs))

(defmethod select-dao ((dao-class symbol) predicate &rest ordering)
  (apply #'select-dao (find-class dao-class) predicate ordering))

(defmethod query-dao ((dao-class symbol) query)
  (query-dao (find-class dao-class) query))

;; ----------------

(defmethod dao-exists-p ((dao-class db-class) key value)
  "Checks if DAO exists using postmodern's QUERY function and clever SQL magic."
  (values (steampunk.database:query (:select (:exists (:select 1
                                                       :from (pomo:dao-table-name dao-class)
                                                       :where (:= key value))))
                                    :single)))

(defmethod create-dao ((dao-class db-class) &rest initargs)
  "Makes DAO using postmodern's MAKE-DAO function."
  (apply #'pomo:make-dao dao-class initargs))

(defmethod select-dao ((dao-class db-class) predicate &rest ordering)
  "Selects DAO from database using postmodern's QUERY-DAO macro."
  (let* ((unordered-query `(:select '*
                            :from ,(pomo:dao-table-name dao-class)
                            :where ,predicate))
         (prepared-query (if ordering
                             `(:order-by ,unordered-query ,@ordering)
                             unordered-query))
         (compiled-query (pomo:sql-compile prepared-query)))
    (postmodern:query-dao dao-class compiled-query)))

(defmethod query-dao ((dao-class db-class) query)
  "Queries DAO from database using postmodern's QUERY-DAO macro."
  (pomo:query-dao dao-class (pomo:sql-compile query)))

(defmethod update-dao ((dao db-object) updates)
  "Updates DAO slots."
  (loop for (slot value) in updates
        do (setf (slot-value dao slot) value)))

(defmethod update-dao :after ((dao db-object) updates)
  "Commits updates to database using postmodern's UPDATE-DAO function."
  (declare (ignorable updates))
  (pomo:update-dao dao))

(defmethod delete-dao ((dao db-object))
  "Deletes DAO from database using postmodern's DELETE-DAO function."
  (pomo:delete-dao dao))

(defmethod link-dao ((dao-1 db-object) slot-1 (dao-2 db-object) slot-2)
  "Checks whether DAO-1 and DAO-2 and non-NIL and then sets slots."
  (when (and dao-1 dao-2)
    (setf (slot-value dao-1 dao-1) (slot-value dao-2 dao-2))))

(defmethod link-dao :after ((dao-1 db-object) slot-1 (dao-2 db-object) slot-2)
  "Commits updates to database using postmodern's UPDATE-DAO function."
  (declare (ignorable slot-1 dao-2 slot-2))
  (pomo:update-dao dao-1))
