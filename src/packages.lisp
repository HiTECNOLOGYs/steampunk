;;;; Package definitions

(defpackage #:steampunk.utilities
  (:documentation "Contains various utility functions that are used throughout the project.")
  (:nicknames #:steam.utilities)

  (:use #:cl
        #:alexandria)

  (:export #:filter              ; Class
           )
  )

(defpackage #:steampunk.connection
  (:documentation "Database connection management code.")
  (:nicknames #:steam.connection #:steam.conn)
  (:use #:cl
        #:alexandria)

  (:use #:cl-annot.class)

  (:export #:database            ; Class
           #:database            ; Function
           #:define-database     ; Function
           #:undefine-database   ; Function
           #:connect-toplevel    ; Function
           #:connected-p         ; Function
           #:connect             ; Function
           #:reconnect           ; Function
           #:disconnect-toplevel ; Function
           #:disconnect          ; Function
           #:with-connection     ; Macro
           )
  )

(defpackage #:steampunk.database
  (:documentation "Mappings to basic database functions.")
  (:nicknames #:steampunk.db #:steam.database #:steam.db)

  (:use #:cl
        #:alexandria)

  (:use #:cl-annot.class)

  (:export #:query                  ; Macro
           #:execute                ; Macro
           #:doquery                ; Macro
           #:prepare                ; Macro
           #:defprepared            ; Macro
           #:defprepared-with-names ; Macro
           #:with-transaction       ; Macro
           #:commit-transaction     ; Function
           #:rollback-transaction   ; Function
           #:with-savepoint         ; Macro
           #:release-savepoint      ; Function
           #:commit-hooks           ; Function
           #:abort-hooks            ; Function
           #:ensure-transaction     ; Macro
           #:list-schemata          ; Function
           #:schemata-exists-p      ; Function
           #:with-schema            ; Macro
           #:create-schema          ; Function
           #:drop-schema            ; Function
           #:search-path            ; Function
           #:list-tables            ; Function
           #:table-exists-p         ; Function
           #:table-description      ; Function
           #:list-sequences         ; Function
           #:sequence-exists-p      ; Function
           #:sequence-next          ; Function
           #:list-views             ; Function
           #:view-exists-p          ; Function
           )
  )

(defpackage #:steampunk.schema
  (:documentation "Schema creation and ORM.")
  (:nicknames #:steam.schema)

  (:use #:cl
        #:alexandria
        #:anaphora)

  (:use #:cl-annot.class)

  (:import-from #:steampunk.utilities
                #:filter)

  ;; Schema creation
  (:export #:init-database   ; Function
           #:deinit-database ; Function
           )

  ;; Sequences
  (:export #:defsequence     ; Macro
           )

  ;; Tables
  (:export #:deftable        ; Macro
           )

  ;; DAOs
  (:export #:defdao          ; Macro
           #:db-object       ; Class
           #:db-class        ; Metaclass
           )

  ;; ORM
  (:export #:defentity       ; Macro
           #:defrelation     ; Macro
           )
  )

(defpackage #:steampunk.operations
  (:documentation "Operations over database object mappings.")
  (:nicknames #:steampunk.ops #:steam.operations #:steam.ops)

  (:use #:cl
        #:alexandria
        #:anaphora)

  (:use #:cl-annot.class)

  (:import-from #:steampunk.schema
                #:db-object
                #:db-class)

  (:export #:dao-exists-p              ; Generic function
           #:create-dao                ; Generic function
           #:select-dao                ; Generic function
           #:query-dao                 ; Generic function
           #:update-dao                ; Generic function
           #:delete-dao                ; Generic function
           #:link-dao                  ; Generic function
           #:database-record-exists-p  ; Function
           #:wrap-dao                  ; Macro
           )
  )

(defpackage #:steampunk
  (:documentation "Main package. Aggregates all the others.")
  (:nicknames #:steam)

  (:use #:cl
        #:alexandria)

  (:use #:steampunk.connection
        #:steampunk.database
        #:steampunk.schema
        #:steampunk.operations)

  (:use #:cl-annot.class)
  )
