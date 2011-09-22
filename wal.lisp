(in-package :wal)

(defstruct wal
  master
  writer
  stream
  entry-writer
  entry-reader
  entries)

(defun default-entry-reader (stream)
  (read stream nil nil))

(defun wal-pathname (master)
  "Returns write ahead log file's pathname. The pathname's base is
taken from master stream's pathname."
  (merge-pathnames (make-pathname :type "wal") (pathname master)))

(defun open-wal-stream (master if-exists if-does-not-exist)
  "Returns open write ahead log stream."
  (if (binary-file:binary-array-stream-p master)
      (binary-file:make-binary-array-io-stream)
      (cl:open (wal-pathname master) :element-type :default :direction :io :if-exists if-exists :if-does-not-exist if-does-not-exist)))

(defun open (master writer &key (entry-writer #'print) (entry-reader #'default-entry-reader) (if-exists :overwrite) (if-does-not-exist :error))
  "Open write ahead log file."
  (make-wal :master master :writer writer :entry-writer entry-writer :entry-reader entry-reader :stream (open-wal-stream master if-exists if-does-not-exist)))

(defun close (wal)
  "Close write ahead log file."
  (cl:close (wal-stream wal)))

(defun write (wal entry)
  "Write log entry to write ahead log."
  (push entry (wal-entries wal))
  (funcall (wal-entry-writer wal) entry (wal-stream wal)))

(defun checkpoint (wal)
  (close wal)
  (unless (binary-file:binary-array-stream-p (wal-stream wal))
    (delete-file (wal-stream wal)))
  (setf (wal-entries wal) nil
        (wal-stream  wal) (open-wal-stream (wal-master wal) :error :create))
  wal)
  
(defun commit (wal)
  "Commit entries. All pending entries are written to master file."
  (print :commit (wal-stream wal))
  (finish-output (wal-stream wal))
  (redo wal)
  (checkpoint wal))

(defun rollback (wal)
  "Rollback enries. Changes are discarded and master file stays untouched."
  (checkpoint wal))

(defun pending-commits-p (wal)
  "Returns t if write ahead log has uncommitted entries, nil otherwise."
  (> (binary-file:binary-stream-length (wal-stream wal)) 0))

(defun redo (wal)
  "Write ahead redo policy. Entries in wal are written to master file."
  (dolist (entry (reverse (wal-entries wal)))
    (funcall (wal-writer wal) entry))
  (finish-output (wal-master wal)))

(defun recover (wal)
  "Recover master file from write ahead log."
  (when (pending-commits-p wal)
    (do ((obj (funcall (wal-entry-reader wal) (wal-stream wal))
              (funcall (wal-entry-reader wal) (wal-stream wal))))
        ((null obj))
      (push obj (wal-entries wal)))
    (when (eql (pop (wal-entries wal)) :commit)
      (redo wal))
    (checkpoint wal)))

