;;;; Common database operations mappings

(in-package #:steampunk.database)
(cl-annot:enable-annot-syntax)

;;; **************************************************************************
;;;  Querying
;;; **************************************************************************

@export
(defmacro query (query &rest args/format)
  "Execute given QUERY."
  `(pomo:query ,query ,@args/format))

@export
(defmacro execute (query &rest args)
  "Execute given QUERY but suppress result."
  `(pomo:execute ,query ,@args))

@export
(defmacro doquery (query (&rest names) &body body)
  "Executes QUERY and iterates over results."
  `(pomo:doquery ,query (,@names)
     ,@body))

@export
(defmacro prepare (query &optional (format :rows))
  "Creates a function that can be used as an interface to QUERY."
  `(pomo:prepare ,query ,format))

@export
(defmacro defprepared (name query &optional (format :rows))
  "DEFUN-style variant of PREPARE."
  `(pomo:defprepared ,name ,query ,format))

@export
(defmacro defprepared-with-names (name (&rest args) (query &rest query-args) &optional (format :rows))
  "DEFPREPARED variant that allows to specify names of arguments."
  `(pomo:defprepared-with-names ,name (,@args)
       (,query ,@query-args)
       ,format))

;;; **************************************************************************
;;;  Transactions
;;; **************************************************************************

@export
(defmacro with-transaction ((&optional name) &body body)
  "Wraps in database transaction."
  `(pomo:with-transaction (,@(when name (list name)))
     ,@body))

@export
(defun commit-transaction (transaction)
  "Commits given database transaction."
  (pomo:commit-transaction transaction))

@export
(defun rollback-transaction (transaction)
  "Roll back given database transaction."
  (pomo:abort-transaction transaction))

@export
(defmacro with-savepoint (name &body body)
  "Used within a transaction to define a savepoint."
  `(pomo:with-savepoint ,name
     ,@body))

@export
(defun release-savepoint (savepoint)
  "Releases given savepoint."
  (pomo:release-savepoint savepoint))

@export
(defun rollback-savepoint (savepoint)
  "Rolls back savepoint."
  (pomo:rollback-savepoint savepoint))

@export
(defun commit-hooks (transaction-or-savepoint)
  "Returns a list of hooks that are executed on transaction or savepoint commit."
  (pomo:commit-hooks transaction-or-savepoint))

@export
(defun (setf commit-hooks) (new-value transaction-or-savepoint)
  "SETF-function for COMMIT-HOOKS."
  (setf (pomo:commit-hooks transaction-or-savepoint) new-value))

@export
(defun abort-hooks (transaction-or-savepoint)
  "Returns a list of hooks that are executed if transaction or savepoint is aborted."
  (pomo:abort-hooks transaction-or-savepoint))

@export
(defun (setf abort-hooks) (new-value transaction-or-savepoint)
  "SETF-function for ABORT-HOOKS."
  (setf (pomo:abort-hooks transaction-or-savepoint) new-value))

@export
(defmacro ensure-transaction (&body body)
  "Checks if BODY is already within transaction. Executes it in one otherwise."
  `(pomo:ensure-transaction
     ,@body))

;;; **************************************************************************
;;;  Schemata
;;; **************************************************************************

@export
(defun list-schemata ()
  "Lists schemata in database."
  (pomo:list-schemata))

@export
(defun schemata-exists-p (name)
  "Checks if table under given NAME exists in database."
  (pomo:schema-exist-p name))

@export
(defmacro with-schema ((namespace &key (strict t) (if-does-not-exist :create) drop-after)
                       &body body)
  "Executes a BODY within NAMESPACE."
  `(pomo:with-schema (,namespace :strict ,strict :if-not-exist ,if-does-not-exist :drop-after ,drop-after)
     ,@body))

@export
(defun create-schema (schema)
  "Creates new schema."
  (pomo:create-schema schema))

@export
(defun drop-schema (schema)
  "Drops existing schema."
  (pomo:drop-schema schema))

@export
(defun search-path ()
  "Returns current search path."
  (pomo:get-search-path))

@export
(defun (setf search-path) (new-value)
  "SETF-function for SEARCH-PATH."
  (pomo:set-search-path new-value))

;;; **************************************************************************
;;;  Database inspection
;;; **************************************************************************

@export
(defun list-tables (&optional as-strings?)
  "Lists tables in database. If AS-STRINGS? is true, then it returns names as strings instead of
keywords."
  (pomo:list-tables as-strings?))

@export
(defun table-exists-p (name)
  "Checks if table under given NAME exists in database."
  (pomo:table-exists-p name))

@export
(defun table-description (name &optional schema-name)
  "Returns a list of columns in table with given NAME.
Every field is a list of the following structure: (NAME TYPE CAN-BE-NULL?)"
  (pomo:table-description name schema-name))

;;; **************************************************************************
;;;  Sequences
;;; **************************************************************************

@export
(defun list-sequences (&optional as-strings?)
  "Lists sequences in database. If AS-STRINGS? is true, then it returns names as strings instead of
keywords."
  (pomo:list-sequences as-strings?))

@export
(defun sequence-exists-p (name)
  "Checks if sequence under given NAME exists in database."
  (pomo:sequence-exists-p name))

@export
(defun sequence-next (sequence)
  "Gets next value from SEQUENCE."
  (pomo:sequence-next sequence))

;;; **************************************************************************
;;;  Views
;;; **************************************************************************

@export
(defun list-views (&optional strings-p)
  "Lists views in database. If AS-STRINGS? is true, then it returns names as strings instead of
keywords."
  (pomo:list-views strings-p))

@export
(defun view-exists-p (name)
  "Checks if view under given NAME exists in database."
  (pomo:view-exists-p name))
