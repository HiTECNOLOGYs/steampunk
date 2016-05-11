(defsystem #:steampunk
  :description "A wrapper over postmodern."
  :long-description #.(uiop:read-file-string
                       (uiop:subpathname *load-pathname* "README.md"))
  :author "Mark Fedurin <me@hitecnologys.org>"
  :maintainer "Mark Fedurin <me@hitecnologys.org>"
  :license "GPLv3"
  :version (:read-file-form "version.lisp-expr")
  :homepage "https://github.com/HiTECNOLOGYs/steampunk"
  :bug-tracker "https://github.com/HiTECNOLOGYs/steampunk/issues"
  :source-control (:git "git@github.com:HiTECNOLOGYs/steampunk.git")
  :depends-on (#:alexandria
               #:anaphora
               #:cl-annot
               #:postmodern
               #:optima)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "utilities")
               (:file "connection")
               (:file "database")
               (:file "schema")
               (:file "operations"))
  :in-order-to ((test-op (test-op #:steampunk/test)))
  :perform (test-op :after (op c)
             (funcall (intern #.(string :run) :prove) c)))

(defsystem #:steampunk/test
  :description "Tests for steampunk."
  :author "Mark Fedurin <me@hitecnologys.org>"
  :maintainer "Mark Fedurin <me@hitecnologys.org>"
  :defsystem-depends-on (#:prove-asdf)
  :depends-on (#:steampunk
               #:prove)
  :serial t
  :pathname "t/"
  :components ((:file "packages")
               (:test-file "steampunk"))
  :perform (test-op :after (op c)
             (funcall (intern #.(string :run) :prove) c)))
