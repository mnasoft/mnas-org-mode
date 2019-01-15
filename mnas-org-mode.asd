;;;; mnas-org-mode.asd

(asdf:defsystem #:mnas-org-mode
  :description "Describe mnas-org-mode here"
  :author   "Nick Matvyeyev <mnasoft@gmail.com>"
  :license  "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later" 
  :version  "0.0.1"
  :serial t
  :depends-on (#:cl-ppcre)
  :components ((:file "package")
               (:file "mnas-org-mode")))
