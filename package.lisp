;;;; package.lisp

(defpackage #:mnas-org-mode
  (:use #:cl)
  (:export
   utime->date make-hiper-link
   day-of-week date->date
   hiper-link->link
   table-to-org
   utime->time
   hiper-link->description
   date-time->utime
   time->time
   utime->date-time
   )
  )

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))

(setf sb-impl::*default-external-format* :utf8)
