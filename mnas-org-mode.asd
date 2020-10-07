;;;; mnas-org-mode.asd

(defsystem "mnas-org-mode"
  :description "Describe mnas-org-mode here"
  :author   "Nick Matvyeyev <mnasoft@gmail.com>"
  :license  "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later" 
  :version  "0.0.2"
  :depends-on ("cl-ppcre"
	        "cl-annot"
	       )
  :serial nil
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-org-mode")))))

