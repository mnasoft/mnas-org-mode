
(defpackage #:mnas-org-mode/docs
  (:use #:cl ) 
  (:nicknames "MOMOD/DOCS")
  (:export make-all)
  (:documentation "Пакет @b(mnas-org-mode/docs) содержит функции
  генерирования и публикации документации."))

(in-package :mnas-org-mode/docs)

(defun make-document ()
  (loop
    :for i :in
    '((:mnas-org-mode          :mnas-org-mode)
      )
    :do (apply #'mnas-package:document i)))

(defun make-graphs ()
  (loop
    :for i :in
    '(:mnas-org-mode
      )
    :do (mnas-package:make-codex-graphs i i)))

(defun make-all (&aux
                   (of (if (find (uiop:hostname)
                                 mnas-package:*intranet-hosts*
                                 :test #'string=)
                           '(:type :multi-html :template :gamma)
                           '(:type :multi-html :template :minima))))
  "@b(Описание:) функция @b(make-all) служит для создания документации.

 Пакет документации формируется в каталоге
~/public_html/Common-Lisp-Programs/mnas-org-mode.
"
  (make-document)
  (make-graphs)
  (mnas-package:make-mainfest-lisp
   '(:mnas-org-mode :mnas-org-mode/docs)
   "Mnas-Org-Mode"
   '("Nick Matvyeyev")
   (mnas-package:find-sources "mnas-org-mode")
   :output-format of)
  (codex:document :mnas-org-mode)
  (make-graphs)
  (mnas-package:copy-doc->public-html "mnas-org-mode")
  (mnas-package:rsync-doc "mnas-org-mode"))

;;;; (make-all)
