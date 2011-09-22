(in-package :common-lisp-user)

;;(require :unit-test)

(defpackage wal
  (:use :cl) ;;:unit-test
  (:shadow #:open #:close #:write)
  (:export #:wal
           #:open 
           #:close
           #:write
           #:commit
           #:rollback
           #:recover))

