;;;; Database connection management

(in-package #:steampunk.connection)
(cl-annot:enable-annot-syntax)

(defparameter *databases* (make-hash-table)
  "Stores table of defined database connections.")

@export-slots
@export
(defclass database ()
  ((name :initarg :name
         :type string)
   (host :initarg :host
         :type string)
   (port :initarg :port
         :initform 5432
         :type fixnum)
   (user :initarg :user
         :initform "postgres"
         :type string)
   (password :initarg :password
             :type string)
   (use-ssl? :initarg :use-ssl?
             :initform nil
             :type boolean)
   (connection :initform nil
               :type (or pomo:database-connection null)))
  (:documentation "Stores information about database.
Used to establish database connection so it also stores connection object in CONNECTION slot which
is set by CONNECT call and should not be touched by the user."))

@export
(defun database (id)
  "Returns database information stores under given ID."
  (gethash id *databases*))

@export
(defun (setf database) (new-value id)
  "SETF-function for DATABASE."
  (setf (gethash id *databases*) new-value))

@export
(defun define-database (id &key name host port user password use-ssl?)
  "Defines new database under given ID."
  (let ((database (make-instance 'database)))
    (when name
      (setf (slot-value database 'name) name))
    (when host
      (setf (slot-value database 'host) host))
    (when port
      (setf (slot-value database 'port) port))
    (when user
      (setf (slot-value database 'user) user))
    (when password
      (setf (slot-value database 'password) password))
    (when use-ssl?
      (setf (slot-value database 'use-ssl?) use-ssl?))
    (setf (database id) database)))

@export
(defun undefine-database (id)
  "Removes definition of database under given ID."
  (remhash id *databases*))

@export
(defun connect-toplevel (database)
  "Establishes toplevel connection to DATABASE.
Toplevel connections are usually not a good idea unless you're doing testing."
  (check-type database database)
  (with-slots (name host port user password use-ssl?) database
    (pomo:connect-toplevel name
                           user
                           password
                           host
                           :port port
                           :use-ssl use-ssl?)))

@export
(defun connected-p (database)
  "Checks if database has already been connected to and returns T if so. Otherwise, returns NIL."
  (with-slots (connection) database
    (when (and (not (null connection)) (pomo:connected-p connection))
      t)))

@export
(defun connect (database)
  "Establishes connection to DATABASE and returns connection object.
Checks if connection has already been established and does nothing if so, otherwise establishes
connection and sets CONNECTION slot of DATABASE to DATABASE-CONNECTION instance."
  (check-type database database)
  (with-slots (name host port user password use-ssl? connection) database
    (unless (connected-p database)
      (let ((pomo-connection (pomo:connect name
                                           user
                                           password
                                           host
                                           :port port
                                           :use-ssl use-ssl?)))
        (setf connection pomo-connection)))))

@export
(defun reconnect (database)
  "Reconnects existing DATABASE connection."
  (with-slots (connection) database
    (pomo:reconnect connection)))

@export
(defun disconnect-toplevel ()
  "Disconnects existing toplevel connection."
  (pomo:disconnect-toplevel))

@export
(defun disconnect (database)
  "Drops DATABASE connection."
  (with-slots (connection) database
    (pomo:disconnect connection)
    (unless (connected-p database)
      (setf connection nil))))

@export
(defmacro with-connection (database &body body)
  "Executes BODY with database CONNECTION.

If DATABASE is a symbol, wraps gets it from list of defined databases.
Otherwise, passes it unmodified to lower level constructs."
  `(pomo:with-connection (slot-value (if (symbolp ,database)
                                         (database ,database)
                                         ,database)
                                     'connection)
     ,@body))
