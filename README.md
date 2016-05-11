Steampunk
=========

This wrapper implements more high-level interface to database with
possiblity to change backend in future releases as it maps all of the
postmodern's functionality to its own abstractions.

The following features are available:

* More advanced database connection management
* Low-level mappings to database functions (``STEAMPUNK.DATABASE``)
* Schema management and ORM (``STEAMPUNK.SCHEMA``)
* Various operations on above mentioned database objects (``STEAMPUNK.OPERATIONS``)

Usage
-----

You can either use low-level mappings if you want compatilibily level
in case Postmodern ever becomes deprecated or you can use my
high-level interface.

For documentation, you can read docstrings or see API reference: <insert link here>

Consider, if you have the energy, the following example. Say, we have two database entities: a user and a password. I don't know why would anyone use different tables to store them but just once let's denormalize them.

We could define a user using the following form:

```lisp
(steampunk.schema:defentity user ()
  ((id :db-type serial)
   (name :initarg :name
         :db-type string))
  ()
  (:table-name users))
```

Then we could define password using the following form:

```lisp
(steampunk.schema:defentity password ()
  ((user-id :db-type integer)
   (password :initarg :password
             :db-type string))
  (user-id -> (user id))
  (:table-name password))
```

Now we need to define a database connection using the following form:

```lisp
(steampunk.connection:define-database :my-database
  :name "my-database"
  :host "localhost"
  :user "postgres"
  :password "my-password")
```

And then we establish connection:

```lisp
(steamunk.connection:connect (steampunk.connection:database :my-database))
```

After it's done, let's initialize our database with necessary tables:

```lisp
(steampunk.connection:with-connection (:my-database)
  (steampunk.schema:init-database))
```

API
---

The following packages are available for usage:

* `STEAMPUNK` (`STEAM`) — main package aggregating all the other packages.
* `STEAMPUNK.CONNECTION` (`STEAM.CONNECTION`, `STEAM.CONN`) — database connection management.
* `STEAMPUNK.DATABASE` (`STEAMPUNK.DB`, `STEAM.DATABASE`, `STEAM.DB`) — direct mappings to most raw postmodern functions.
* `STEAMPUNK.SCHEMA` (`STEAM.SCHEMA`) — schema management and ORM.
* `STEAMPUNK.OPERATION` (`STEAMPUNK.OPS`, `STEAM.OPERATIONS`, `STEAM.OPS`) — operations over ORM objects.

Installation
------------

(It's not yet in Quicklisp but I plan on submitting pull request)

If you have Quicklisp installed, then use:

```lisp
(ql:quickload :steampunk)
```

Otherwise, install Quicklisp and proceed to the previous paragraph.

Authors
-------

* Mark Fedurin (me@hitecnologys.org)
