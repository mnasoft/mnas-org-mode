;;;; mnas-org-mode.asd

(defsystem "mnas-org-mode"
  :description "Describe mnas-org-mode here"
  :author   "Nick Matvyeyev <mnasoft@gmail.com>"
  :license  "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later" 
  :version  "0.0.3"
  :depends-on ("cl-ppcre" "mnas-string")
  :serial nil
  :components
  ((:file "package")
   (:module "src" :depends-on ("package")
    :serial nil
    :components
    ((:file "mnas-org-mode")))))

(defsystem "mnas-org-mode/docs"
  :description "Зависимости для сборки документации"
  :author "Nick Matvyeyev <mnasoft@gmail.com>"
  :license "GNU GENERAL PUBLIC LICENSE Version 3, 29 June 2007 or later"
  :depends-on ("mnas-org-mode" "codex" "mnas-package")
  :components ((:module "src/docs"
		:serial nil
                :components ((:file "docs")))))
