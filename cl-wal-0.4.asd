(in-package :common-lisp-user)

(defpackage :wal.system
  (:use :asdf :cl)
  (:export #:cl-wal-0.4))

(in-package :wal.system)

(defsystem cl-wal-0.4
   :name "Simple write ahead log for Common Lisp."
   :description "Write Ahead Log writes entries first to a journal and then to master stream."
   :version "0.4"
   :author "Sami Makinen <sami.o.makinen@gmail.com>"
   :components ((:file "package") 
                (:file "wal" :depends-on ("package"))
                (:module "vendor"
                         :components ((:file "lisp-unit")))
                (:module "test" :depends-on ("wal" "vendor")
                         :components
                         ((:module "unit" 
                                   :components ((:file "wal"))))))
   :depends-on (:cl-binary-file-0.4))
