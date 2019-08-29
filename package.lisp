;;;; package.lisp

(defpackage #:mnas-org-mode)

(defpackage #:mnas-org-mode
  (:use #:cl)
  (:intern mnas-org-mode::*hl-sample*
	   mnas-org-mode::*l-sample*
           mnas-org-mode::*d-sample*
	   )
  (:intern mnas-org-mode::*day-of-week*
           mnas-org-mode::*d-sample*
	   )
  (:export mnas-org-mode::hiper-link->link
	   mnas-org-mode::hiper-link->description
	   mnas-org-mode:: make-hiper-link
	   )
  (:intern mnas-org-mode::*date-sample*
	   mnas-org-mode::*time-sample*
	   )
  (:export mnas-org-mode::date->date
	   mnas-org-mode::time->time
	   mnas-org-mode::date-time->utime
	   mnas-org-mode::day-of-week
	   mnas-org-mode::utime->date
	   mnas-org-mode::utime->time))

;;;; (declaim (optimize (compilation-speed 0) (debug 3) (safety 0) (space 0) (speed 0)))
